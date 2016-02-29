{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- | LDIF serializers
module Text.LDIF.Printer (
	ldif2str,
        ver2str,
        dn2str,
	record2str
)
where
import Prelude
import Text.LDIF.Types
import Text.LDIF.Consts
import qualified Data.ByteString.Char8 as BC
import Data.List as L
import Data.Char
import Numeric (showHex)

-- | Serialize LDIF in LDIF Format
ldif2str :: LDIF -> BC.ByteString
ldif2str (LDIF v xs) = BC.unlines $ (ver2str v) ++ (map (record2str) xs)

-- | Serialize version to LDIF Format Lines
ver2str :: Maybe BC.ByteString -> [BC.ByteString]
ver2str Nothing = []
ver2str (Just v) = ["version: " `BC.append` v]

-- | Serialize DN to LDIF Format
dn2str :: DN -> BC.ByteString
dn2str xs = BC.intercalate "," $ map (\((Attribute n),v) -> n `BC.append` "="  `BC.append` (escapeDNVals $ aVal v)) (dnAttrVals xs)
  where
    escapeDNVals :: BC.ByteString -> BC.ByteString
    escapeDNVals vs = BC.concat $ map escapeDNVal (BC.unpack vs)
      where
        escapeDNVal x | not $ isPrint x          = BC.pack $ '\\':(showHex (ord x) "")
                      | elem x escapedDNChars  = BC.pack $ '\\':[x]
                      | otherwise                = BC.pack $ [x]

-- | Serialize Content Record in LDIF Format
record2str :: LDIFRecord -> BC.ByteString
record2str (ContentRecord dn xs)                = BC.unlines $ [ "dn: " `BC.append` (dn2str dn) ] ++ (attrVals2Ln xs)
record2str (ChangeRecord dn (ChangeDelete))     = BC.unlines   [ "dn: " `BC.append` (dn2str dn), "changetype: delete" ]
record2str (ChangeRecord dn (ChangeAdd xs))     = BC.unlines $ [ "dn: " `BC.append` (dn2str dn), "changetype: add"    ] ++ (attrVals2Ln xs)
record2str (ChangeRecord dn (ChangeModify xs))  = BC.unlines $ [ "dn: " `BC.append` (dn2str dn), "changetype: modify" ] ++ (mods2Ln xs)
record2str (ChangeRecord dn (ChangeModDN))      = BC.unlines $ [ "dn: " `BC.append` (dn2str dn), "changetype: moddn"  ]

attrVals2Ln :: [AttrValue] -> [BC.ByteString]
attrVals2Ln xs = map (attrVal2Ln) xs

attrVal2Ln :: AttrValue -> BC.ByteString
attrVal2Ln ((Attribute n),v) = BC.concat [ n,": ", aVal v ]

mods2Ln :: [Modify] -> [BC.ByteString]
mods2Ln xs = L.intercalate ["-"] $ map (mod2Ln) xs
  where
    mod2Ln :: Modify -> [BC.ByteString]
    mod2Ln (ModAdd     a zs) = [ attrVal2Ln ((Attribute "add"),Value $ aName a)     ] ++ (map (\v -> attrVal2Ln (a,v)) zs) 
    mod2Ln (ModDelete  a zs) = [ attrVal2Ln ((Attribute "delete"),Value $ aName a)  ] ++ (map (\v -> attrVal2Ln (a,v)) zs)
    mod2Ln (ModReplace a zs) = [ attrVal2Ln ((Attribute "replace"),Value $ aName a) ] ++ (map (\v -> attrVal2Ln (a,v)) zs)
