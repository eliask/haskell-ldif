{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- | LDIF related operations
module Text.LDIF.Utils 
       where
import Prelude
import Text.LDIF.Types
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

type LDIFCache = M.Map DN LDIFRecord

createLookupTable :: LDIF -> LDIFCache
createLookupTable (LDIF _ xs) = M.fromList $ map (\x -> (reDN x, x)) xs

-- | Find all Contents with given DN
findRecordsByDN :: LDIF -> DN -> [LDIFRecord]
findRecordsByDN (LDIF _ entries) dn = filter (\x -> (reDN x) == dn) entries

-- | Find first Content with given DN
findRecordByDN :: LDIFCache -> DN -> Maybe LDIFRecord
findRecordByDN cache dn = M.lookup dn cache

-- | Find fist Attribute within attributes pairs list
lookupAttr :: BC.ByteString -> [AttrValue] -> Maybe Value
lookupAttr attr xs = lookup (Attribute attr) xs

-- | Filter Attribute Value list according Attribute name
filterAttr :: BC.ByteString -> [AttrValue] -> [AttrValue]
filterAttr attr xs  = filter (\x -> (Attribute attr) == fst x) xs

-- | Change record without any impact
isDummyRecord :: LDIFRecord -> Bool
isDummyRecord (ChangeRecord _ (ChangeModify [])) = True 
isDummyRecord _ = False

leafOfDN :: DN -> AttrValue
leafOfDN xs = getDNValue xs 0

rootOfDN :: DN -> AttrValue
rootOfDN xs = getDNValue xs ((lengthOfDN xs)-1)

lengthOfDN :: DN -> Int
lengthOfDN xs = length (dnAttrVals xs)

getDNValue :: DN -> Int -> AttrValue
getDNValue xs idx = (dnAttrVals xs) !! idx

takeDNPrefix :: DN -> Int -> DN
takeDNPrefix (DN vals) n  = (DN (reverse $ take n (reverse vals)))

-- | Check if the dn1 is prefix of dn2
isDNPrefixOf :: DN -> DN -> Bool
isDNPrefixOf dn1 dn2 | (lengthOfDN dn1) >= (lengthOfDN dn2) = False
                     | otherwise = let n = (lengthOfDN dn1)
                                   in (takeDNPrefix dn2 n) == dn1

isParentRecordOf :: LDIFRecord -> LDIFRecord -> Bool
isParentRecordOf a b = isDNPrefixOf (reDN a) (reDN b)


-- | Make LDIF Values case-insensitive
ldif2ldifI :: LDIF -> LDIF
ldif2ldifI = undefined
