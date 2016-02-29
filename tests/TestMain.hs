import Text.LDIF
import Test.HUnit
import Data.Either
import Data.List
import System.Directory
import System.FilePath
import Control.Monad (liftM)

ldifDir = "data"

main = do
    ls <- getLDIFs ldifDir
    runTestTT (tests ls)

tests ls = TestList $ (testCasesParseOK ls) ++ testCasesDIFF ++ testCasesUtils ++ (testCasesPrintOK ls)

--
-- Test Cases
--
testCasesDIFF = [TestCase (assertEqual "dummy" True True)]
testCasesParseOK ls = map (\x -> TestCase (checkParsing x)) $ filter (isOK) ls
    where
      checkParsing fname = do
        ret <- parseLDIFFile fname
        assertParsedOK fname ret "Parsing test"

testCasesPrintOK ls = map (\x -> TestCase (checkParsing x)) $ filter (isOK) ls
    where
      checkParsing fname = do
        ret <- parseLDIFFile fname
        case ret of 
          Left _     -> return ()
          Right ldif -> do
              let ret2 = parseLDIFStr (ldif2str ldif)
              assertParsedOK fname ret2 "Printing test"

testCasesUtils = [ TestCase (assertBool "DN1Root is Prefix of DN2Root" (not $ isDNPrefixOf dn1root dn2root))
                 , TestCase (assertBool "DN1Root is Prefix of DN1Child" (isDNPrefixOf dn1root dn1child))
                 , TestCase (assertBool "DN Size 1" (1 == sizeOfDN dn1root))
                 , TestCase (assertBool "DN Size 2" (2 == sizeOfDN dn1child))]
    where
      dn1root = head $ rights [ parseDNStr "dc=sk" ]
      dn2root = head $ rights [ parseDNStr "dc=de" ]
      dn1child = head $ rights [ parseDNStr "o=green,dc=sk" ]
--
-- Support Methods
--
getLDIFs :: String -> IO [String]
getLDIFs dr = do
    liftM (map (dr </>)) $ liftM (filter isLDIF) $ getDirectoryContents dr
  
isOK x = isPrefixOf "OK" (takeFileName x)
isLDIF x = isSuffixOf ".ldif" x

assertParsedOK filename ret msg  = case ret of
  Left e -> assertFailure $ msg ++ " " ++ (show e)
  Right ldif -> assertParsedType filename ldif

assertParsedType name ldif | (isSuffixOf ".modify.ldif" name) = assertTypeChanges name ldif
                           | (isSuffixOf ".content.ldif" name) = assertTypeContent name ldif
                           | otherwise = assertFailure $ "Unexpected filename: (not .modify.ldif or .content.ldif " ++ name

assertTypeContent n l  | (getLDIFType l == LDIFContentType) = assertBool "Valid Content Type" True -- >> (putStrLn $ "\n\n" ++ n ++ "\n\n" ++ (show l))
                       | otherwise          = assertFailure $ n ++ " is not type of LDIFContent"

assertTypeChanges n l | (getLDIFType l /= LDIFContentType) = assertBool "Valid Changes Type" True -- >> (putStrLn $ "\n\n" ++ n ++ "\n\n" ++ (show l))
                      | otherwise          = assertFailure $ n ++ " is not type of LDIFChanges"
  
