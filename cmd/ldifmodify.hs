{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- | Apply LDAP operations within LDIF on another LDIF.
-- | Without schema related verification like syntax, cardinality etc.
import Data.List
import Data.Either
import Data.Maybe
import Control.Monad 
import System.FilePath 
import System.Environment
import Text.LDIF
import System.Console.CmdArgs
import qualified Data.ByteString.Char8 as BC

progDesc = "Apply LDAP operations from LDIF to LDIF (like ldapmodify)"

data LdifModify = LdifModify { baseFile :: FilePath
                             , modFiles  :: [FilePath]
                             , outFile  :: FilePath } deriving (Show, Data, Typeable)

defaultCfg = LdifModify { baseFile = def &= typFile &= name "f" &= help "Base LDIF File"
                        , modFiles = def &= args &= typ "LDIF Files for applying"
                        , outFile = def &= typFile &= name "o" &= help "Output LDIF File" }

main = do
  cfg <- cmdArgs defaultCfg
  execute cfg

execute (LdifModify [] _ _) = putStrLn "Error: -f base LDIF File is mandatory"
execute (LdifModify _ [] _) = putStrLn "Error: no LDIF Files for applying provided"
execute (LdifModify _ _ []) = putStrLn "Error: -o output LDIF File is mandatory"
execute cfg = do
  baseLDIF <- safeParseLDIFFile (baseFile cfg)
  modLDIFs <- mapM (safeParseLDIFFile) (modFiles cfg)
  let outLDIF = foldr (flip applyLDIF) baseLDIF modLDIFs
  if length (outFile cfg) == 0 then do
      BC.putStrLn (ldif2str outLDIF)
    else do
      BC.writeFile (outFile cfg) (ldif2str outLDIF)
      putStrLn $ (outFile cfg) ++ " written."

safeParseLDIFFile :: FilePath -> IO LDIF
safeParseLDIFFile name = liftM (either (\e -> error $ "Can not parse: "++(show e)) (id)) (parseLDIFFile defaulLDIFConf name)
