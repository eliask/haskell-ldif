{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Make Change LDIF based on two Content LDIFs
import Data.List
import Data.Either
import Data.Maybe
import Control.Monad 
import System.FilePath 
import System.Environment
import Text.LDIF
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BC

-- | Serialize DN to LDIF Format
dn2html :: DN -> BC.ByteString
dn2html ys@(DN xs) = BC.concat [ "<div id=\"abc" 
                               , nm 
                               , "\"></div><b>dn:</b> <font color=\"green\"><b>" 
                               , dnstr 
                               , "</b></font>" ]
      where
        dnstr = BC.intercalate "," $ map (\((Attribute n),v) -> n `BC.append` "=" `BC.append` (aVal v)) xs
        nm = aVal $ dn2last ys

-- | Serialize Change Record in LDIF Format
record2html :: Set.Set Value -> LDIFRecord -> BC.ByteString
record2html idx (ChangeRecord dn (ChangeDelete))     = BC.unlines   [ (dn2str dn), "changetype: delete" ]
record2html idx (ChangeRecord dn (ChangeAdd xs))     = BC.unlines $ [ (dn2html dn), "changetype: add"    ] ++ (attrVals2Ln idx xs)
record2html idx (ChangeRecord dn (ChangeModify xs))  = BC.unlines $ [ (dn2str dn), "changetype: modify" ] ++ (mods2Ln idx xs)
record2html idx (ChangeRecord dn (ChangeModDN))      = BC.unlines $ [ (dn2str dn), "changetype: moddn"  ]
record2html idx (ContentRecord dn xs) = BC.unlines $ [ (dn2html dn) ] ++ (attrVals2Ln idx xs)

attrVals2Ln :: Set.Set Value -> [AttrValue] -> [BC.ByteString]
attrVals2Ln idx xs = map (attrVal2Ln idx) xs

attrVal2Ln :: Set.Set Value -> AttrValue -> BC.ByteString
attrVal2Ln idx ((Attribute n),v) = if Set.member v idx then BC.concat [ "<b>"
                                                                      , n 
                                                                      , "</b> : <a href=\"#abc"
                                                                      , aVal v
                                                                      , "\">" 
                                                                      , aVal v
                                                                      , "</a>"]
                          else BC.concat [ "<b>",n,"</b> : ",aVal v]

mods2Ln :: Set.Set Value -> [Modify] -> [BC.ByteString]
mods2Ln idx xs = intercalate ["-"] $ map (mod2Ln idx) xs

mod2Ln :: Set.Set Value -> Modify -> [BC.ByteString]
mod2Ln idx mod = [ attrVal2Ln idx (modName,attrName) ] ++ valLst
  where
    attrName = Value $ aName $ modAttr mod
    valLst = map (\v -> attrVal2Ln idx (modAttr mod,v)) $ modAttrVals mod
    modName = case mod of 
      (ModAdd _ _)     -> Attribute "add"
      (ModDelete _ _)  -> Attribute "delete"
      (ModReplace _ _) -> Attribute "replace"

dns :: LDIF -> Set.Set Value
dns (LDIF _ xs) = Set.fromList $ nub $ map (dn2last . reDN) xs

dn2last (DN xs) = snd $ head xs

main = do
  args <- getArgs
  case args of 
     []         -> putStrLn "Usage: ldif2html <input.ldif> [<input2.ldif> <input3.ldif> ... <inputN.ldif> <output.ldif>]"
     [inp]      -> do
                      doLDIF2HTML [inp] (BC.putStrLn)
                      putStrLn "# Done."
     xs         -> do
                      doLDIF2HTML (init xs) (BC.writeFile (last xs)) 
                      putStrLn $ "## Processed : " ++ (unlines $ init xs)
                      putStrLn $ "## Written   : " ++ (last xs)
  where
    doLDIF2HTML names outF = do
      mls <- mapM (parseLDIFFile defaulLDIFConf) names
      case lefts mls of 
        []   -> do
                let idx = Set.unions $ map dns $ rights mls
                outF $ BC.concat [ "<html><body bgcolor=lightyellow><pre>"
                                 , BC.concat $ map (ldif2html idx) (rights mls) 
                                 , "</pre></body></html>" ]
        xs  -> mapM_ print xs
      -- | Serialize LDIF to HTML
    ldif2html :: Set.Set Value -> LDIF -> BC.ByteString
    ldif2html idx (LDIF v xs) = BC.unlines $ (ver2str v) ++ (map (record2html idx) xs)


