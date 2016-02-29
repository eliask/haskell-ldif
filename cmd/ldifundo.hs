{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
import Data.List
import Data.Either
import Data.Maybe
import Control.Monad
import System.FilePath
import System.Environment
import Text.LDIF
import System.Console.CmdArgs
import qualified Data.ByteString.Char8 as BC

progDesc = "Calculate undo LDIF (rollback LDIF)"

data LdifUndo = LdifUndo { inFile   :: FilePath
                         , outFile  :: FilePath } deriving (Show, Data, Typeable)

defaultCfg = LdifUndo { inFile = def &= typFile &= name "f" &= help "Input LDIF File"
                      , outFile = def &= typFile &= name "o" &= help "Output LDIF File" }

main = do
  cfg <- cmdArgs defaultCfg
  execute cfg

execute (LdifUndo [] _) = putStrLn "Error: -f base LDIF File is mandatory"
execute cfg = do
  inLDIF <- safeParseLDIFFile (inFile cfg)
  let (outLDIF, wrn) = undoLDIF inLDIF
  when (not $ null wrn) (putStrLn $ "Finished with warnings: " ++ (unlines $ map unlines wrn))
  if length (outFile cfg) == 0 then do
      BC.putStrLn (ldif2str outLDIF)
    else do
      BC.writeFile (outFile cfg) (ldif2str outLDIF)
      putStrLn $ (outFile cfg) ++ " written."

safeParseLDIFFile :: FilePath -> IO LDIF
safeParseLDIFFile name = liftM (either (\e -> error $ "Can not parse: "++(show e)) (id)) (parseLDIFFile defaulLDIFConf name)
