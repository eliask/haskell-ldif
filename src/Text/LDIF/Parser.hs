module Text.LDIF.Parser (
	parseLDIFStr,
        parseLDIFStrAs,
	parseLDIFFile,
        parseDNStr
)
where
import Text.LDIF.Types
import Text.ParserCombinators.Parsec
import Data.Char
import Data.List (isPrefixOf)

-- | Parse string as LDIF content and return LDIF or ParseError
parseLDIFStr :: String -> Either ParseError LDIF
parseLDIFStr = parseLDIFStrAs Nothing 

-- | Read and parse provided file and return LDIF or ParseError
parseLDIFFile :: String -> IO (Either ParseError LDIF)
parseLDIFFile name = do
	input <- readFile name
        return $ parseLDIFStrAs' name Nothing input

-- | Read and parse provided string and return LDIF or ParserError
-- | If LDIF type is specified than given type is expected for parsing 
-- | and mismatch generates ParseError
parseLDIFStrAs' :: String -> Maybe LDIFType -> String -> Either ParseError LDIF
parseLDIFStrAs' nm Nothing                = parse pLdif        nm . preproc
parseLDIFStrAs' nm (Just LDIFMixedType)   = parse pLdif        nm . preproc
parseLDIFStrAs' nm (Just LDIFContentType) = parse pLdifContent nm . preproc
parseLDIFStrAs' nm (Just LDIFChangesType) = parse pLdifChanges nm . preproc

parseLDIFStrAs :: Maybe LDIFType -> String -> Either ParseError LDIF
parseLDIFStrAs = parseLDIFStrAs' "(param)"

-- | Parse string as DN and return DN type or ParseError
parseDNStr :: String -> Either ParseError DN
parseDNStr = parse pDN "(param)" 

-- | Preprocessing for concat wrapped lines and remove comment lines
preproc :: String -> String
preproc = stripComments . unwrap

-- | Remove Comment Lines
stripComments :: String -> String
stripComments input = unlines $ filter (not . isPrefixOf "#") $ lines input

-- | Unwrap lines, lines with space at begin is continue of previous line 
unwrap :: String -> String
unwrap xs = unlines $ takeLines $ lines xs

takeLines :: [String] -> [String]
takeLines [] = []
takeLines xs = let (ln,ys) = takeLine xs
               in ln:takeLines ys

takeLine :: [String] -> (String, [String])
takeLine []     = ([],[])
takeLine (x:[]) = (x,[])
takeLine (x:xs) = let isCont z = " " `isPrefixOf` z
                  in (x ++ (concat $ map (tail) $ takeWhile (isCont) xs), dropWhile (isCont) xs) 

-- | Parsec ldif parser
pLdif :: CharParser st LDIF
pLdif = try pLdifChanges <|> try pLdifMixed

pLdifChanges :: CharParser st LDIF
pLdifChanges = do
    pSEPs
    ver <- optionMaybe pVersionSpec
    pSEPs
    recs <- sepEndBy1 pChangeRec pSEPs
    eof
    return $ LDIF ver recs

pLdifMixed:: CharParser st LDIF
pLdifMixed = do
    pSEPs
    ver <- optionMaybe pVersionSpec
    pSEPs
    recs <- sepEndBy1 pRec pSEPs
    eof
    return $ LDIF ver recs

pLdifContent :: CharParser st LDIF
pLdifContent = do
    pSEPs
    ver <- optionMaybe pVersionSpec
    pSEPs
    recs <- sepEndBy1 pAttrValRec pSEPs
    eof
    return $ LDIF ver recs

pAttrValRec ::  CharParser st LDIFRecord
pAttrValRec = do
    dn <- pDNSpec
    pSEP
    attrVals <- sepEndBy1 pAttrValSpec pSEP
    return $ ContentRecord dn attrVals

pRec :: CharParser st LDIFRecord
pRec = try pChangeRec <|> pAttrValRec

pChangeRec :: CharParser st LDIFRecord
pChangeRec = try pChangeAdd
         <|> try pChangeDel
         <|> try pChangeMod
         <|> pChangeModDN

pChangeAdd :: CharParser st LDIFRecord
pChangeAdd = do
    dn <- pDNSpec
    pSEP
    _ <- string "changetype:"
    pFILL
    _ <- string "add"
    pSEP
    vals <- sepEndBy1 pAttrValSpec pSEP
    return $ ChangeRecord dn (ChangeAdd vals)

pChangeDel :: CharParser st LDIFRecord
pChangeDel = do
    dn <- pDNSpec
    pSEP
    _ <- string "changetype:"
    pFILL
    _ <- string "delete"
    pSEP
    return $ ChangeRecord dn ChangeDelete

pChangeMod :: CharParser st LDIFRecord
pChangeMod = do
    dn <- pDNSpec
    pSEP
    _ <- string "changetype:"
    pFILL
    _ <- string "modify"
    pSEP
    mods <- sepEndBy1 pModSpec (char '-' >> pSEP)
    return $ ChangeRecord dn (ChangeModify mods)

pChangeModDN :: CharParser st LDIFRecord
pChangeModDN = do
    dn <- pDNSpec
    pSEP
    _ <- string "changetype:"
    pFILL
    _ <- string "modrdn" 
    pSEP
    _ <- string "newrdn:"
    pFILL 
    _ <- pRDN
    pSEP
    _ <- string "deleteoldrdn:"
    pFILL
    _ <- oneOf "01"
    pSEP
    return $ ChangeRecord dn ChangeModDN

pRDN :: CharParser st String
pRDN = pSafeString

pDNSpec :: CharParser st DN
pDNSpec = do
    _ <- string "dn:"
    pDN

pDN :: CharParser st DN
pDN = do
   pFILL
   avals <- sepEndBy pAttrEqValue (char ',')  
   return $ DN avals

pAttrEqValue :: CharParser st AttrValue
pAttrEqValue = do
   pFILL
   att <- pAttributeType
   _ <- char '='
   val <- pAttrValueDN
   return (att,val)

pAttrValueDN :: CharParser st Value
pAttrValueDN = do
   many (noneOf stringChars)
   where 
     specialChars = [',','=','+','#',';','\n','\r']
     -- specialChars = [',','=','+','<','>','#',';','\n','\r']
     stringChars  = '\\':'"':specialChars 

pVersionSpec :: CharParser st String
pVersionSpec = do
   _ <- string "version:"
   pFILL
   many1 digit

pModSpec :: CharParser st Modify
pModSpec = do
   modType <- pModType
   pFILL
   att <- pAttributeDescription 
   pSEP 
   vals <- sepEndBy pAttrValSpec pSEP
   return $ mkMod modType att vals

mkMod :: String -> Attribute -> [AttrValue] -> Modify
mkMod modType att vals | modType == "add:" = ModAdd att (map (snd) vals)
                       | modType == "delete:" = ModDelete att (map (snd) vals)
                       | modType == "replace:" = ModReplace att (map (snd) vals)
                       | otherwise = error $ "unexpected mod:" ++ modType 
                         -- error can not be reached because pModType

pModType :: CharParser st String
pModType = try (string "add:")
       <|> try (string "delete:")
       <|> string "replace:"

pAttributeDescription :: CharParser st Attribute
pAttributeDescription = pAttributeType

pAttributeType :: CharParser st Attribute
pAttributeType = try pLdapOid
             <|> (do { l <- letter; o <- pAttrTypeChars; return (Attribute $ l:o) } )

pAttrValSpec :: CharParser st AttrValue
pAttrValSpec = do
   name <- pAttributeDescription
   val  <- pValueSpec
   return (name, val)

pValueSpec :: CharParser st Value
pValueSpec = try (char ':' >> char ':' >> pFILL >> pBase64String)
         <|> try (char ':' >> pFILL >> pSafeString') 
         <|> (char ':' >> char '<' >> pFILL >> pURL)

pURL :: CharParser st String
pURL = pSafeString

pSafeString :: CharParser st String
pSafeString = do
   c <- noneOf "\n\r :<"
   r <- many (noneOf "\n\r")
   return $ c:r

pSafeString' :: CharParser st String
pSafeString' = do
   r <- many (noneOf "\n\r")
   return r
 
pBase64String :: CharParser st String
pBase64String = pSafeString

pAttrTypeChars :: CharParser st String
pAttrTypeChars = many (satisfy (\x -> isAlphaNum x || x == '-'))

pLdapOid :: CharParser st Attribute
pLdapOid = do
   num <- many1 digit
   rest <- many (do { _ <- string "."; n <- many1 digit; return $ '.':n})
   return (Attribute $ num ++ concat rest)

pFILL :: CharParser st ()
pFILL = skipMany (oneOf [' ', '\t'])

pSEP :: CharParser st ()
pSEP = try (char '\r' >> char '\n' >> return () )
   <|> (char '\n' >> return () )

pSEPs :: CharParser st ()
pSEPs = many pSEP >> return ()

