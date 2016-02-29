-- | LDIF serializers
module Text.LDIF.Printer (
	ldif2Str,
        dn2Str,
	change2Str,
	content2Str
)
where
import Text.LDIF.Types
import Data.List

-- | Serialize LDIF in LDIF Format
ldif2Str :: LDIF -> String
ldif2Str (LDIFContent v xs) = unlines $ (ver2Str v) ++ (map (content2Str) xs)
ldif2Str (LDIFChanges v xs) = unlines $ (ver2Str v) ++ (map (change2Str) xs)

-- | Serialize version to LDIF Format Lines
ver2Str :: Maybe String -> [String]
ver2Str Nothing = []
ver2Str (Just v) = ["version: "++v]

-- | Serialize DN to LDIF Format
dn2Str :: DN -> String
dn2Str (DN xs) = intercalate "," $ map (\(n,v) -> n++"="++v) xs

-- | Serialize Content Record in LDIF Format
content2Str :: ContentRecord -> String
content2Str (ContentRecord dn xs) = unlines $ [ "dn: "++(dn2Str dn) ] ++ (attrVals2Ln xs)

-- | Serialize Change Record in LDIF Format
change2Str :: ChangeRecord -> String
change2Str (ChangeRecord dn (ChangeDelete))     = unlines   [ "dn: "++(dn2Str dn), "changetype: delete" ]
change2Str (ChangeRecord dn (ChangeAdd xs))     = unlines $ [ "dn: "++(dn2Str dn), "changetype: add"    ] ++ (attrVals2Ln xs)
change2Str (ChangeRecord dn (ChangeModify xs))  = unlines $ [ "dn: "++(dn2Str dn), "changetype: modify" ] ++ (mods2Ln xs)
change2Str (ChangeRecord dn (ChangeModDN))      = unlines $ [ "dn: "++(dn2Str dn), "changetype: moddn"  ]

attrVals2Ln :: [AttrValue] -> [String]
attrVals2Ln xs = map (attrVal2Ln) xs

attrVal2Ln :: AttrValue -> String
attrVal2Ln (n,v) = n ++ ": "++v

mods2Ln :: [Modify] -> [String]
mods2Ln xs = intercalate ["-"] $ map (mod2Ln) xs

mod2Ln :: Modify -> [String]
mod2Ln (ModAdd     nm xs) = [ attrVal2Ln ("add",nm)     ] ++ (map (\v -> attrVal2Ln (nm,v)) xs) 
mod2Ln (ModDelete  nm xs) = [ attrVal2Ln ("delete",nm)  ] ++ (map (\v -> attrVal2Ln (nm,v)) xs)
mod2Ln (ModReplace nm xs) = [ attrVal2Ln ("replace",nm) ] ++ (map (\v -> attrVal2Ln (nm,v)) xs)
