-- | Make Change LDIF based on two Content LDIFs
import Data.List
import Data.Either
import Data.Maybe
import Control.Monad 
import System.FilePath 
import System.Environment
import Text.LDIF
import qualified Data.Set as Set

ldif2html :: Set.Set String -> LDIF -> String
ldif2html idx (LDIFContent v xs) = unlines $ (ver2Str v) ++ (map (content2html idx) xs)
ldif2html idx (LDIFChanges v xs) = unlines $ (ver2Str v) ++ (map (change2html idx) xs)

-- | Serialize version to LDIF Format Lines
ver2Str :: Maybe String -> [String]
ver2Str Nothing = []
ver2Str (Just v) = ["version: "++v]

-- | Serialize DN to LDIF Format
dn2html :: DN -> String
dn2html ys@(DN xs) = concat [ "<div id=\"abc"++nm++"\"></div><b>dn:</b> <font color=\"green\"><b>", intercalate "," $ map (\(n,v) -> n++"="++v) xs, "</b></font>"]
      where
         nm = dn2last ys

dn2last (DN xs) = snd $ head xs

-- | Serialize Content Record in LDIF Format
content2html :: Set.Set String -> ContentRecord -> String
content2html idx (ContentRecord dn xs) = unlines $ [ (dn2html dn) ] ++ (attrVals2Ln idx xs)

-- | Serialize Change Record in LDIF Format
change2html :: Set.Set String -> ChangeRecord -> String
change2html idx (ChangeRecord dn (ChangeDelete))     = unlines   [ (dn2Str dn), "changetype: delete" ]
change2html idx (ChangeRecord dn (ChangeAdd xs))    = unlines $ [ (dn2html dn), "changetype: add"    ] ++ (attrVals2Ln idx xs)
change2html idx (ChangeRecord dn (ChangeModify xs))  = unlines $ [ (dn2Str dn), "changetype: modify" ] ++ (mods2Ln idx xs)
change2html idx (ChangeRecord dn (ChangeModDN))      = unlines $ [ (dn2Str dn), "changetype: moddn"  ]

attrVals2Ln :: Set.Set String -> [AttrValue] -> [String]
attrVals2Ln idx xs = map (attrVal2Ln idx) xs

attrVal2Ln :: Set.Set String -> AttrValue -> String
attrVal2Ln idx (n,v) = if Set.member v idx then "<b>"++n++"</b> : <a href=\"#abc"++v++"\">"++v++"</a>"
                          else "<b>"++n++"</b> : "++v

mods2Ln :: Set.Set String -> [Modify] -> [String]
mods2Ln idx xs = intercalate ["-"] $ map (mod2Ln idx) xs

mod2Ln :: Set.Set String -> Modify -> [String]
mod2Ln idx (ModAdd     nm xs) = [ attrVal2Ln idx ("add",nm)     ] ++ (map (\v -> attrVal2Ln idx (nm,v)) xs) 
mod2Ln idx (ModDelete  nm xs) = [ attrVal2Ln idx ("delete",nm)  ] ++ (map (\v -> attrVal2Ln idx (nm,v)) xs)
mod2Ln idx (ModReplace nm xs) = [ attrVal2Ln idx ("replace",nm) ] ++ (map (\v -> attrVal2Ln idx (nm,v)) xs)

-- Dummy ldif2html implementation but it should
-- display LDIF file as HTML with values as href to
-- DN which has the value as the leaf RDN value
main = do
  args <- getArgs
  case args of 
     []         -> putStrLn "Usage: ldif2html <input.ldif> [<input2.ldif> <input3.ldif> ... <inputN.ldif> <output.ldif>]"
     [inp]      -> do
                      doLDIF2HTML [inp] (putStrLn)
                      putStrLn "# Done."
     xs         -> do
                      doLDIF2HTML (init xs) (writeFile (last xs)) 
                      putStrLn $ "## Processed : " ++ (unlines $ init xs)
                      putStrLn $ "## Written   : " ++ (last xs)

doLDIF2HTML names outF = do
  mls <- mapM (parseLDIFFile) names
  case lefts mls of 
       []   -> do
                 let idx = Set.unions $ map (dns) (rights mls)
                 outF $ concat ["<html><body bgcolor=lightyellow><pre>", concat $ map (ldif2html idx) (rights mls), "</pre></body></html>" ]
       xs  -> mapM_ print xs

dns :: LDIF -> Set.Set String
dns (LDIFContent _ xs) = Set.fromList $ nub $ map (dn2last . coDN) xs
dns (LDIFChanges _ xs) = Set.fromList $ nub $ map (dn2last . chDN) xs
