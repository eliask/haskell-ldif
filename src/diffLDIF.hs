{-# LANGUAGE DeriveDataTypeable #-}
-- | Make Change LDIF based on two Content LDIFs
import Data.List
import Data.Either
import Data.Maybe
import Control.Monad 
import System.FilePath 
import System.Environment
import Text.LDIF
import System.Console.CmdArgs

progDesc = "Create delta LDIF between Source LDIF and Target LDIF"

data DiffLdif = DiffLdif { srcFile :: FilePath
                         , dstFile :: FilePath } deriving (Show, Data, Typeable)

defaultCfg = mode $ DiffLdif { srcFile = def &= typFile & flag "s" & text "Source LDIF File"
                             , dstFile = def &= typFile & flag "t" & text "Target LDIF File" }

verifyCfg :: DiffLdif -> IO ()
verifyCfg (DiffLdif [] []) = do 
  msg <- cmdArgsHelp progDesc [defaultCfg] Text
  error msg
verifyCfg (DiffLdif [] _)  = error "Missing Source LDIF File parameter (-s)"
verifyCfg (DiffLdif _ [])  = error "Missing Target LDIF File parameter (-t)"
verifyCfg (DiffLdif _ _ )  = return ()

main = do
  cfg <- cmdArgs progDesc [defaultCfg]
  verifyCfg cfg
  ml1 <- parseLDIFFile (srcFile cfg)
  ml2 <- parseLDIFFile (dstFile cfg)
  case rights [ml1,ml2] of 
       [l1,l2] -> case diffLDIF l1 l2 of
                    Left err -> putStrLn err
                    Right delta -> putStrLn $ ldif2str delta
       _       -> print $ lefts [ml1,ml2] 
