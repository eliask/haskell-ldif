import Text.LDIF
import Test.HUnit
import Data.Either
import Data.List
import System.Directory
import System.FilePath
import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as BC

-- | Directory with input LDIF files for tests
ldifDir = "tests/data"

main = do
    ls <- getLDIFs ldifDir
    runTestTT (tests ls)
  where
    tests ls = TestList $ (testCasesParseOK ls) 
               ++ testCasesDiff
               ++ testCasesApply
               ++ testCasesTree
               ++ testCasesUtils 
               ++ (testCasesPrintOK ls)

--
-- Test Cases
--
testCasesDiff  = [ TestCase (assertEqual "dummy" True True) ] -- TODO: Implement!
testCasesApply = [ TestCase (assertEqual "dummy" True True) ] -- TODO: Implement!
testCasesTree  = [ TestCase (assertEqual "dummy" True True) ] -- TODO: Implement!

-- | Parser Tests
testCasesParseOK ls = map (TestCase . checkParsing) $ filter isSuccessFile ls
    where
      checkParsing fname = do
        ret <- parseLDIFFile defaulLDIFConf fname
        assertParsedOK fname ret "Parsing test"

-- | Printer Tests
testCasesPrintOK ls = map (TestCase . checkParsing) $ filter isSuccessFile ls
    where
      checkParsing fname = do
        ret <- parseLDIFFile defaulLDIFConf fname
        either (\_ -> return ()) printAndParseAgain ret
          where
            printAndParseAgain ldif = do
              let ret2 = parseLDIFStr defaulLDIFConf fname (ldif2str ldif)
              assertParsedOK fname ret2 "Printing test"

-- | Utils Tests
testCasesUtils = [ TestCase $ assertBool "DN1Root is Prefix of DN2Root"  $ not $ dn1root `isDNPrefixOf` dn2root
                 , TestCase $ assertBool "DN1Root is Prefix of DN1Child" $ dn1root `isDNPrefixOf` dn1child
                 , TestCase $ assertBool "DN Size 1" $ 1 == lengthOfDN dn1root
                 , TestCase $ assertBool "DN Size 2" $ 2 == lengthOfDN dn1child 
                 ]
    where
      dn1root  = parseRightDN "dc=sk"
      dn2root  = parseRightDN "dc=de"
      dn1child = parseRightDN "o=green,dc=sk"
      parseRightDN dstr = fromRight $ parseDNStr defaulLDIFConf $ BC.pack dstr
        where
          fromRight (Left e)  = error $ show e
          fromRight (Right d) = d

-- | Support Methods
getLDIFs :: String -> IO [String]
getLDIFs dr = do
    liftM (map (dr </>)) $ liftM (filter isLDIF) $ getDirectoryContents dr
      where
        isLDIF x = isSuffixOf ".ldif" x  
        
-- | Assert that file is parsed successfully and correct LDIF type
assertParsedOK name ret msg  = either onError onSuccess ret
  where
    onSuccess l = assertParsedType name l
    onError   e = assertFailure $ msg ++ " " ++ show e 

-- | Assert that file is parsed with expected LDIF type
assertParsedType name l = assertType $ contentTypeFile name
  where
    assertType t | getLDIFType l == t = assertBool "Valid Content Type" True
                 | otherwise          = let msg = name ++ " is not type of " ++ (show t) ++ " but " ++ (show $ getLDIFType l)
                                        in assertFailure msg

-- | Files which names begin with "OK" are expected to be parsed Successfully
isSuccessFile x = isPrefixOf "OK" (takeFileName x)

-- | Files suffixes defines what is expected parsed content type
contentTypeFile x | isModifyFile  x = LDIFChangesType
                  | isContentFile x = LDIFContentType
                  | otherwise       = error $ "Invalid filename format in tests: " ++ x
  where
    isModifyFile x  = isSuffixOf ".modify.ldif"  x
    isContentFile x = isSuffixOf ".content.ldif" x
