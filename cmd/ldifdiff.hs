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
import qualified Data.ByteString.Char8 as BC

progDesc = "Create delta LDIF between Source LDIF and Target LDIF"

data LdifDiff = LdifDiff { srcFile :: FilePath
                         , dstFile :: FilePath } deriving (Show, Data, Typeable)

defaultCfg = LdifDiff { srcFile = def &= typFile &= name "s" &= help "Source LDIF File"
                      , dstFile = def &= typFile &= name "t" &= help "Target LDIF File" }

main = do
  cfg <- cmdArgs defaultCfg
  execute cfg

execute (LdifDiff []  _  ) = putStrLn "Error: -s source file is mandatory" 
execute (LdifDiff _ []   ) = putStrLn "Error: -t target file is mandatory"
execute (LdifDiff src dst) = do
  ml1 <- parseLDIFFile defaulLDIFConf src
  ml2 <- parseLDIFFile defaulLDIFConf dst
  case rights [ml1,ml2] of 
       [l1,l2] -> case diffLDIF l1 l2 of
                    Left err -> putStrLn err
                    Right delta -> BC.putStrLn $ ldif2str delta
       _       -> print $ lefts [ml1,ml2] 
