{-# LANGUAGE DeriveDataTypeable #-}
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

data LdifModify = LdifModify { baseFile :: FilePath
                             , modFiles  :: [FilePath]
                             , outFile  :: FilePath } deriving (Show, Data, Typeable)

defaultCfg = mode $ LdifModify { baseFile = def &= typFile & flag "f" & text "Base LDIF File"
                               , modFiles = def &= args & typ "LDIF Files for applying"
                               , outFile = def &= typFile & flag "o" & text "Output LDIF File" }

verifyCfg :: LdifModify -> IO ()
verifyCfg (LdifModify bf mfx ouf) | length bf  == 0   = error "Missing Base LDIF File parameter (-f)"
                                  | length mfx == 0   = error "Missing LDIF Files for applying as arguments"
                                  | otherwise         = return ()                                                              

main = do
  cfg <- cmdArgs "LDIF Modify. Apply LDAP operations from LDIF to LDIF" [defaultCfg]
  verifyCfg cfg
  baseLDIF <- safeParseLDIFFile (baseFile cfg)
  modLDIFs <- mapM (safeParseLDIFFile) (modFiles cfg)
  let outLDIF = foldr (flip applyLDIF) baseLDIF modLDIFs
  if length (outFile cfg) == 0 then do
      putStrLn (ldif2str outLDIF)
    else do
      writeFile (outFile cfg) (ldif2str outLDIF)
      putStrLn $ (outFile cfg) ++ " written."

safeParseLDIFFile :: FilePath -> IO LDIF
safeParseLDIFFile name = liftM (either (\e -> error $ "Can not parse: "++(show e)) (id)) (parseLDIFFile name)
