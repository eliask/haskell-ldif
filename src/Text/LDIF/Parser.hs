module Text.LDIF.Parser (
	parseLDIFStr,
	parseLDIFFile,
        parseDNStr
)
where
import Text.LDIF.Types
import Text.ParserCombinators.Parsec
import Data.Either
import Data.Char
import Data.List (isPrefixOf)

-- | Parse string as LDIF content and return LDIF or ParseError
parseLDIFStr :: String -> Either ParseError LDIF
parseLDIFStr = parse pLdif "(param)" . preproc 

-- | Read and parse provided file and return LDIF or ParseError
parseLDIFFile :: String -> IO (Either ParseError LDIF)
parseLDIFFile name = do
	input <- readFile name
	return $ parse pLdif name (preproc input)

-- | Parse string as DN and return DN type or ParseError
parseDNStr :: String -> Either ParseError DN
parseDNStr = parse pDN "(param)" 

-- | Preprocessing for concat wrapped lines and remove comment lines
preproc :: String -> String
preproc = unwrap . stripComments

-- | Remove Comment Lines
stripComments :: String -> String
stripComments input = unlines $ filter (not . isPrefixOf "#") $ lines input

-- | Unwrap lines, lines with space at begin is continue of previous line 
unwrap :: String -> String
unwrap input = unlines $ preprocLines $ lines input
   where 
    preprocLines xs = unbox $ foldl (preprocLine) ([],Nothing) xs
    preprocLine (ys,r) []                 = (addLineMaybe ys r,Just []) 
    preprocLine (ys,r) (l:lx) | l == ' '  = (ys,concatLineMaybe r lx)
                              | otherwise = (addLineMaybe ys r, Just $ l:lx)
    concatLineMaybe Nothing  x = Just x
    concatLineMaybe (Just y) x = Just (y++x)
    addLineMaybe xs Nothing  = xs
    addLineMaybe xs (Just x) = xs++[x]
    unbox (ys,Nothing) = ys
    unbox (ys,Just x)  = ys++[x]

-- | Parsec ldif parser
pLdif :: CharParser st LDIF
pLdif = try pLdifChanges <|> pLdifContent

pLdifChanges :: CharParser st LDIF
pLdifChanges = do
    ver <- optionMaybe pVersionSpec
    recs <- sepEndBy1 pChangeRec pSEPs
    return $ LDIFChanges ver recs

pLdifContent :: CharParser st LDIF
pLdifContent = do
    ver <- optionMaybe pVersionSpec
    recs <- sepEndBy1 pAttrValRec pSEPs
    return $ LDIFContent ver recs

pAttrValRec ::  CharParser st ContentRecord
pAttrValRec = do
    dn <- pDNSpec
    pSEP
    attrVals <- sepEndBy1 pAttrValSpec pSEP
    return $ ContentRecord dn attrVals

pChangeRec :: CharParser st ChangeRecord
pChangeRec = try pChangeAdd
         <|> try pChangeDel
         <|> try pChangeMod
         <|> pChangeModDN

pChangeAdd :: CharParser st ChangeRecord
pChangeAdd = do
    dn <- pDNSpec
    pSEP
    string "changetype:"
    pFILL
    string "add"
    pSEP
    vals <- sepEndBy1 pAttrValSpec pSEP
    return $ ChangeRecord dn (ChangeAdd vals)

pChangeDel :: CharParser st ChangeRecord
pChangeDel = do
    dn <- pDNSpec
    pSEP
    string "changetype:"
    pFILL
    string "delete"
    pSEP
    return $ ChangeRecord dn ChangeDelete

pChangeMod :: CharParser st ChangeRecord
pChangeMod = do
    dn <- pDNSpec
    pSEP
    string "changetype:"
    pFILL
    string "modify"
    pSEP
    mods <- sepEndBy1 pModSpec (char '-' >> pSEP)
    return $ ChangeRecord dn (ChangeModify mods)

pChangeModDN :: CharParser st ChangeRecord
pChangeModDN = do
    dn <- pDNSpec
    pSEP
    string "changetype:"
    pFILL
    string "modrdn" 
    pSEP
    string "newrdn:"
    pFILL 
    pRDN
    pSEP
    string "deleteoldrdn:"
    pFILL
    oneOf "01"
    pSEP
    return $ ChangeRecord dn ChangeModDN

pRDN :: CharParser st String
pRDN = pSafeString

pDNSpec :: CharParser st DN
pDNSpec = do
    string "dn:"
    pDN

pDN :: CharParser st DN
pDN = do
   pFILL
   avals <- sepEndBy pAttrEqValue (char ',')  
   return $ DN avals

pAttrEqValue :: CharParser st AttrValue
pAttrEqValue = do
   att <- pAttributeType
   char '='
   val <- pAttrValueDN
   return (att,val)

pAttrValueDN :: CharParser st Value
pAttrValueDN = do
   many (noneOf stringChars)
   where 
     specialChars = [',','=','+','<','>','#',';','\n','\r']
     stringChars  = '\\':'"':specialChars 

pVersionSpec :: CharParser st String
pVersionSpec = do
   string "version:"
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

mkMod :: String -> String -> [AttrValue] -> Modify
mkMod modType att vals | modType == "add:" = ModAdd att (map (snd) vals)
                       | modType == "delete:" = ModDelete att (map (snd) vals)
                       | modType == "replace:" = ModReplace att (map (snd) vals)
                       | otherwise = error $ "unexpected mod:" ++ modType 
                         -- error can not be reached because pModType

pModType :: CharParser st String
pModType = try (string "add:")
       <|> try (string "delete:")
       <|> string "replace:"

pAttributeDescription :: CharParser st String
pAttributeDescription = pAttributeType

pAttributeType :: CharParser st String
pAttributeType = try pLdapOid
             <|> (do { l <- letter; o <- pAttrTypeChars; return $ l:o } )

pAttrValSpec :: CharParser st AttrValue
pAttrValSpec = do
   name <- pAttributeDescription
   val  <- pValueSpec
   return (name, val)

pValueSpec :: CharParser st Value
pValueSpec = try (char ':' >> char ':' >> pFILL >> pBase64String)
         <|> try (char ':' >> pFILL >> pSafeString) 
         <|> (char ':' >> char '<' >> pFILL >> pURL)

pURL :: CharParser st String
pURL = pSafeString

pSafeString :: CharParser st String
pSafeString = do
   c <- noneOf "\n\r :<"
   r <- many (noneOf "\n\r")
   return $ c:r
 
pBase64String :: CharParser st String
pBase64String = pSafeString

pAttrTypeChars :: CharParser st String
pAttrTypeChars = many (satisfy (\x -> isAlphaNum x || x == '-'))

pLdapOid :: CharParser st String
pLdapOid = do
   num <- many1 digit
   rest <- many (do { string "."; n <- many1 digit; return $ '.':n})
   return $ num ++ concat rest

pFILL :: CharParser st ()
pFILL = spaces

pSEP :: CharParser st ()
pSEP = try (char '\r' >> char '\n' >> return () )
   <|> (char '\n' >> return () )

pSEPs :: CharParser st ()
pSEPs = many pSEP >> return ()

