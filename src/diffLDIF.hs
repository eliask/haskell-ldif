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

defaultCfg = DiffLdif { srcFile = def &= typFile &= name "s" &= help "Source LDIF File"
                      , dstFile = def &= typFile &= name "t" &= help "Target LDIF File" }

main = do
  cfg <- cmdArgs defaultCfg
  execute cfg

execute (DiffLdif []  _  ) = putStrLn "Error: -s source file is mandatory" 
execute (DiffLdif _ []   ) = putStrLn "Error: -t target file is mandatory"
execute (DiffLdif src dst) = do
  ml1 <- parseLDIFFile src
  ml2 <- parseLDIFFile dst
  case rights [ml1,ml2] of 
       [l1,l2] -> case diffLDIF l1 l2 of
                    Left err -> putStrLn err
                    Right delta -> putStrLn $ ldif2str delta
       _       -> print $ lefts [ml1,ml2] 
