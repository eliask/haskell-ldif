-- | LDIF related operations
module Text.LDIF.Utils (
        findRecordsByDN,
	findRecordByDN,
        isDNPrefixOf,
        sizeOfDN,
        takeDNPrefix,
        leafOfDN,
        rootOfDN,
        lookupAttr,
        filterAttr,
        isDummyRecord,
        ldif2tree,
        getLDIFType,
        isContentRecord,
        isChangeRecord,
        dn2dnI,
        ldif2ldifI
)
where
import Text.LDIF.Types
import Data.Tree

-- | Find all Contents with given DN
findRecordsByDN :: LDIF -> DN -> [LDIFRecord]
findRecordsByDN (LDIF _ entries) dn = filter (\x -> (reDN x) == dn) entries

-- | Find first Content with given DN
findRecordByDN :: LDIF -> DN -> Maybe LDIFRecord
findRecordByDN ldif dn = case findRecordsByDN ldif dn of
                                 []   -> Nothing
                                 xs   -> Just (head xs)


-- | Find fist Attribute within attributes pairs list
lookupAttr :: String -> [AttrValue] -> Maybe Value
lookupAttr attr xs = lookup (Attribute attr) xs

-- | Filter Attribute Value list according Attribute name
filterAttr :: String -> [AttrValue] -> [AttrValue]
filterAttr attr xs  = filter (\x -> (Attribute attr) == fst x) xs

-- | Change record without any impact
isDummyRecord :: LDIFRecord -> Bool
isDummyRecord (ChangeRecord _ (ChangeModify [])) = True 
isDummyRecord _ = False

leafOfDN :: DN -> AttrValue
leafOfDN xs = getDNValue xs 0

rootOfDN :: DN -> AttrValue
rootOfDN xs = getDNValue xs ((sizeOfDN xs)-1)

sizeOfDN :: DN -> Int
sizeOfDN xs = length (dnAttrVals xs)

getDNValue :: DN -> Int -> AttrValue
getDNValue xs idx = (dnAttrVals xs) !! idx

takeDNPrefix :: DN -> Int -> DN
takeDNPrefix (DN vals) n  = (DN (reverse $ take n (reverse vals)))
takeDNPrefix (DNi vals) n = (DNi (reverse $ take n (reverse vals)))

-- | Check if the dn1 is prefix of dn2
isDNPrefixOf :: DN -> DN -> Bool
isDNPrefixOf dn1 dn2 | (sizeOfDN dn1) >= (sizeOfDN dn2) = False
                     | otherwise = let n = (sizeOfDN dn1)
                                   in (takeDNPrefix dn2 n) == dn1

dummyRootDN :: DN
dummyRootDN = DN [(Attribute "dc", "root")]

ldif2tree :: LDIF -> Tree LDIFRecord
ldif2tree (LDIF _ entries) = Node (ChangeRecord dummyRootDN (ChangeAdd [])) (ldifRecs2tree entries)

isParentRecordOf :: LDIFRecord -> LDIFRecord -> Bool
isParentRecordOf a b = isDNPrefixOf (reDN a) (reDN b)

ldifRoots :: [LDIFRecord] -> [LDIFRecord]
ldifRoots xs = let isRoot x = all (\y -> not $ isParentRecordOf y x) xs
               in filter (isRoot) xs

ldifRecs2tree :: [LDIFRecord] -> [Tree LDIFRecord]
ldifRecs2tree xs = let roots = (ldifRoots xs)
                       subtr x = ldifRecs2tree $ filter (isParentRecordOf x) xs
                   in map (\x -> Node x (subtr x)) roots

isContentRecord :: LDIFRecord -> Bool
isContentRecord (ContentRecord _ _) = True
isContentRecord _ = False

isChangeRecord :: LDIFRecord -> Bool
isChangeRecord (ChangeRecord _ _) = True
isChangeRecord _ = False

getLDIFType :: LDIF -> LDIFType
getLDIFType (LDIF _ []) = LDIFContentType
getLDIFType (LDIF _ xs) = getLDIFType' con chg
    where
      con = filter (isContentRecord) xs
      chg = filter (not . isContentRecord) xs
      getLDIFType' [] [] = error "Unexpected"
      getLDIFType' [] _  = LDIFChangesType
      getLDIFType' _  [] = LDIFContentType
      getLDIFType' _  _  = LDIFMixedType

dn2dnI :: DN -> DN
dn2dnI (DN xs) = (DNi xs)
dn2dnI xs = xs

ldif2ldifI :: LDIF -> LDIF
ldif2ldifI (LDIF v xs) = LDIF v ys
    where
      ys = map (\x -> x { reDN = dn2dnI (reDN x) } )  xs
