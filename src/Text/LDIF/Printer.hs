-- | LDIF serializers
module Text.LDIF.Printer (
	ldif2str,
        dn2str,
	record2str
)
where
import Text.LDIF.Types
import Text.LDIF.Consts
import Data.List
import Data.Char
import Numeric (showHex)

-- | Serialize LDIF in LDIF Format
ldif2str :: LDIF -> String
ldif2str (LDIF v xs) = unlines $ (ver2str v) ++ (map (record2str) xs)

-- | Serialize version to LDIF Format Lines
ver2str :: Maybe String -> [String]
ver2str Nothing = []
ver2str (Just v) = ["version: "++v]

-- | Serialize DN to LDIF Format
dn2str :: DN -> String
dn2str xs = intercalate "," $ map (\((Attribute n),v) -> n++"="++(escapeDNVals v)) (dnAttrVals xs)

escapeDNVals :: String -> String
escapeDNVals vs = concat $ map escapeDNVal vs
  where
    escapeDNVal x | not $ isPrint x          = '\\':(showHex (ord x) "")
                  | elem x escapedDNChars    = '\\':[x]
                  | otherwise                = [x]

-- | Serialize Content Record in LDIF Format
record2str :: LDIFRecord -> String
record2str (ContentRecord dn xs)                = unlines $ [ "dn: "++(dn2str dn) ] ++ (attrVals2Ln xs)
record2str (ChangeRecord dn (ChangeDelete))     = unlines   [ "dn: "++(dn2str dn), "changetype: delete" ]
record2str (ChangeRecord dn (ChangeAdd xs))     = unlines $ [ "dn: "++(dn2str dn), "changetype: add"    ] ++ (attrVals2Ln xs)
record2str (ChangeRecord dn (ChangeModify xs))  = unlines $ [ "dn: "++(dn2str dn), "changetype: modify" ] ++ (mods2Ln xs)
record2str (ChangeRecord dn (ChangeModDN))      = unlines $ [ "dn: "++(dn2str dn), "changetype: moddn"  ]

attrVals2Ln :: [AttrValue] -> [String]
attrVals2Ln xs = map (attrVal2Ln) xs

attrVal2Ln :: AttrValue -> String
attrVal2Ln ((Attribute n),v) = n ++ ": "++v

mods2Ln :: [Modify] -> [String]
mods2Ln xs = intercalate ["-"] $ map (mod2Ln) xs

mod2Ln :: Modify -> [String]
mod2Ln (ModAdd     a@(Attribute nm) xs) = [ attrVal2Ln ((Attribute "add"),nm)     ] ++ (map (\v -> attrVal2Ln (a,v)) xs) 
mod2Ln (ModDelete  a@(Attribute nm) xs) = [ attrVal2Ln ((Attribute "delete"),nm)  ] ++ (map (\v -> attrVal2Ln (a,v)) xs)
mod2Ln (ModReplace a@(Attribute nm) xs) = [ attrVal2Ln ((Attribute "replace"),nm) ] ++ (map (\v -> attrVal2Ln (a,v)) xs)
