{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Text.LDIF.Parser (
	parseLDIFStr,
	parseLDIFFile,
        parseDNStr,
        preproc,
        defaulLDIFConf,
        LDIFParserConfig(..)
)
where
import Prelude
import Text.LDIF.Types
import Text.LDIF.Consts
import Text.LDIF.Preproc
import Text.Parsec as PR
import Text.Parsec.ByteString
import Text.Parsec.Pos (initialPos)
import Text.Parsec.Error (Message(..), newErrorMessage)
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Data.Maybe (fromJust, isNothing)
import Numeric (readHex)

-- | LDIF Parser configuration
data LDIFParserConfig = LDIFParserConfig { lpExpectedType :: Maybe LDIFType  -- ^ Type of LDIF expected
                                         , lpCaseSensitive :: Bool } -- ^ Parse as Case Sensitive LDIF
                      deriving Show

-- | Default configuration for parser (Any LDIF Type, Case Sensitive)
defaulLDIFConf :: LDIFParserConfig
defaulLDIFConf = LDIFParserConfig Nothing True

-- | Parse LDIF content
parseLDIFStr :: LDIFParserConfig -> FilePath -> BC.ByteString -> Either ParseError LDIF
parseLDIFStr conf name xs = case eldif of 
                               Left err  -> Left $ transposePos ptab err
                               Right ldf -> checkExpectedType ldf
  where 
    (input, ptab) = preproc xs
    eldif = parse (pLdif conf) name input
    checkExpectedType ldf | (isNothing $ lpExpectedType conf) = Right ldf
                          | (getLDIFType ldf) == (fromJust $ lpExpectedType conf) = Right ldf
                          | otherwise = Left $ newErrorMessage (UnExpect "Invalid LDIF Type") (initialPos name)                                                                                    

-- | Parse LDIF file
parseLDIFFile :: LDIFParserConfig -> FilePath -> IO (Either ParseError LDIF)
parseLDIFFile conf name = do
	input <- BC.readFile name
        return $ parseLDIFStr conf name input

-- | Parse DN
parseDNStr :: LDIFParserConfig -> BC.ByteString -> Either ParseError DN
parseDNStr conf = parse (pDN conf) "(param)" 

-- | Parsec ldif parser
pLdif :: LDIFParserConfig -> Parser LDIF
pLdif conf = do
    pSEPs conf
    ver <- optionMaybe pVersionSpec
    recs <- sepEndBy (pRec conf) (pSEPs1 conf)
    _ <- optionMaybe pSearchResult
    eof
    recs `seq` return $ LDIF ver recs
  where
    pVersionSpec :: Parser BC.ByteString
    pVersionSpec = do
      _ <- string "version:"
      pFILL conf
      xs <- many1 digit
      pSEPs1 conf
      let ys = xs `seq` BC.pack xs
      ys `seq` return $ ys
    pSearchResult :: Parser ()
    pSearchResult = do
      _ <- string "search:"
      pFILL conf
      _ <- many1 digit
      pSEP conf
      _ <- string "result:"
      pFILL conf
      _ <- pSafeString conf
      pSEPs conf
      return ()

pRec :: LDIFParserConfig -> Parser LDIFRecord
pRec conf = do 
  dn <- pDNSpec
  pSEP conf
  try (pChangeRec dn) <|> (pAttrValRec dn)
    where
      pDNSpec :: Parser DN
      pDNSpec = do
        _ <- string "dn:"
        pDN conf
      pAttrValRec :: DN -> Parser LDIFRecord
      pAttrValRec dn = do
        attrVals <- sepEndBy1 (pAttrValSpec conf) (pSEP conf)
        attrVals `seq` return $ ContentRecord dn attrVals
      pChangeRec :: DN -> Parser LDIFRecord
      pChangeRec dn = do
        _ <- string "changetype:"
        pFILL conf
        try (pChangeAdd conf dn)
          <|> try (pChangeDel conf dn)
          <|> try (pChangeMod conf dn)
          <|> (pChangeModDN conf dn)
                      
pChangeAdd :: LDIFParserConfig -> DN -> Parser LDIFRecord
pChangeAdd conf dn  = do
    _ <- string "add"
    pSEP conf
    vals <- sepEndBy1 (pAttrValSpec conf) (pSEP conf)
    return $ ChangeRecord dn (ChangeAdd vals)

pChangeDel :: LDIFParserConfig -> DN -> Parser LDIFRecord
pChangeDel conf dn = do
    _ <- string "delete"
    pSEP conf
    return $ ChangeRecord dn ChangeDelete

pChangeMod :: LDIFParserConfig -> DN -> Parser LDIFRecord
pChangeMod conf dn = do
    _ <- string "modify"
    pSEP conf
    mods <- sepEndBy1 (pModSpec conf) (char '-' >> pSEP conf)
    return $ ChangeRecord dn (ChangeModify mods)

pChangeModDN :: LDIFParserConfig -> DN -> Parser LDIFRecord
pChangeModDN conf dn = do
    _ <- string "modrdn" 
    pSEP conf
    _ <- string "newrdn:"
    pFILL conf
    _ <- pRDN conf
    pSEP conf
    _ <- string "deleteoldrdn:"
    pFILL conf
    _ <- oneOf "01"
    pSEP conf
    return $ ChangeRecord dn ChangeModDN

pRDN :: LDIFParserConfig -> Parser BC.ByteString
pRDN conf = pSafeString conf

pDN :: LDIFParserConfig -> Parser DN
pDN conf = do
   pFILL conf
   avals <- sepEndBy (pAttrEqValue conf) (char ',')
   avals `seq` return $ DN avals

pAttrEqValue :: LDIFParserConfig -> Parser AttrValue
pAttrEqValue conf = do
   pFILL conf
   att <- pAttributeType conf
   _ <- char '='
   val <- pAttrValueDN conf
   att `seq` val `seq` return (att,val)

pAttrValueDN :: LDIFParserConfig -> Parser Value
pAttrValueDN conf = do
   xs <- many1 allChar
   let ys = xs `seq` (mkVal conf $ BC.pack xs)
   ys `seq` return $ ys
   where 
     allChar = noneOf (escapedDNChars ++ "\n")
               <|> try (hexChar)
               <|> (escChar)
     escChar = do
       _ <- char '\\'
       oneOf escapedDNChars
     hexChar = do
       _ <- char '\\'
       hval <- PR.count 2 hexDigit
       case readHex hval of
         [(val,[])] -> return $ chr val
         _          -> fail $ "invalid hex value: " ++ hval

pModSpec :: LDIFParserConfig -> Parser Modify
pModSpec conf = do
   modType <- pModType conf
   pFILL conf
   att <- pAttributeDescription conf
   pSEP conf
   vals <- sepEndBy (pAttrValSpec conf) (pSEP conf)
   return $ mkMod modType att vals

mkMod :: String -> Attribute -> [AttrValue] -> Modify
mkMod modType att vals | modType == "add:" = ModAdd att (map (snd) vals)
                       | modType == "delete:" = ModDelete att (map (snd) vals)
                       | modType == "replace:" = ModReplace att (map (snd) vals)
                       | otherwise = error $ "unexpected mod:" ++ modType
                         -- error can not be reached because pModType

pModType :: LDIFParserConfig -> Parser String
pModType _ = try (string "add:")
       <|> try (string "delete:")
       <|> string "replace:"

pAttributeDescription :: LDIFParserConfig -> Parser Attribute
pAttributeDescription conf = pAttributeType conf

pAttributeType :: LDIFParserConfig -> Parser Attribute
pAttributeType _ = try pLdapOid
             <|> pCharType
   where
      pDotOid = do
         _ <- string "." 
         n <- many1 digit
         let xs = n `seq` '.':n
         xs `seq` return xs
      pLdapOid = do
        num <- many1 digit
        rest <- many1 pDotOid
        let xs = num `seq` rest `seq` num ++ concat rest
        xs `seq` return (Attribute $ BC.pack xs)
      pCharType = do
         l <- letter 
         o <- pAttrTypeChars
         let xs = l `seq` o `seq` l `BC.cons` o
         xs `seq` return $ Attribute xs
           where
             pAttrTypeChars :: Parser BC.ByteString
             pAttrTypeChars = do 
               xs <- many (satisfy (\x -> isAlphaNum x || x == '-'))
               let ys = xs `seq` BC.pack xs
               ys `seq` return ys


pAttrValSpec :: LDIFParserConfig -> Parser AttrValue
pAttrValSpec conf = do
   name <- pAttributeDescription conf
   val  <- pValueSpec
   name `seq` val `seq` return (name, val)
     where
       pValueSpec :: Parser Value
       pValueSpec = try (char ':' >> pFILL conf >> pSafeString' conf >>= (\x -> return $ mkVal conf x))
                    <|> try (char ':' >> char ':' >> pFILL conf >> pBase64String conf >>= (\x -> return $ mkVal conf x))
                    <|> (char ':' >> char '<' >> pFILL conf >> pURL conf >>= (\x -> return $ mkVal conf x))

pURL :: LDIFParserConfig -> Parser BC.ByteString
pURL conf = pSafeString conf

pSafeString :: LDIFParserConfig -> Parser BC.ByteString
pSafeString _ = do
   c <- noneOf "\n :<"
   r <- many (noneOf "\n")   
   let xs = r `seq` c:r
   let ys = xs `seq` BC.pack xs
   ys `seq` return ys

pSafeString' :: LDIFParserConfig -> Parser BC.ByteString
pSafeString' _ = do
   r <- many (noneOf "\n")
   let ys = r `seq` BC.pack r
   ys `seq` return ys
 
pBase64String :: LDIFParserConfig -> Parser BC.ByteString
pBase64String conf = pSafeString conf

pFILL :: LDIFParserConfig -> Parser ()
pFILL _ = skipMany (oneOf [' ', '\t'])

pSEP :: LDIFParserConfig -> Parser ()
pSEP _ = do
         _ <- newline
         return ()

pSEPs :: LDIFParserConfig -> Parser ()
pSEPs conf = many (pSEP conf) >> return ()

pSEPs1 :: LDIFParserConfig -> Parser ()
pSEPs1 conf = many1 (pSEP conf) >> return ()

mkVal :: LDIFParserConfig -> BC.ByteString -> Value
mkVal conf v | (lpCaseSensitive conf) = Value v
             | otherwise              = ValueI v
