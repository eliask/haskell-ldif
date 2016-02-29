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
  ml2 <- parseLDIFFile (args !! 1)
  case rights [ml1,ml2] of 
       [l1,l2] -> putStrLn $ ldif2str $ fromJust $ diffLDIF l1 l2  
       _       -> print $ lefts [ml1,ml2] 
