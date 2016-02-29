-- | Make Change LDIF based on two Content LDIFs
import Data.List
import Data.Either
import Data.Maybe
import Control.Monad 
import System.FilePath 
import System.Environment
import Text.LDIF

main = do
  args <- getArgs
  ml1 <- parseLDIFFile (args !! 0)
  case ml1 of 
       Right l1 -> do
             print l1
             putStrLn "========================================================"
             putStrLn $ ldif2str l1
       Left err -> print err
