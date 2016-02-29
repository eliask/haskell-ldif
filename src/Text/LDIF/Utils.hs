-- | LDIF related operations
module Text.LDIF.Utils (
        findRecordsByDN,
	findRecordByDN,
        isDNPrefixOf,
        sizeOfDN,
        takeDNPrefix,
        leafOfDN,
        rootOfDN,
        isDummyRecord
)
where
import Text.LDIF.Types
import Text.LDIF.Printer
import Data.Maybe
import Data.Either
import Data.List (nub)

-- | Find all Contents with given DN
findRecordsByDN :: LDIF -> DN -> [LDIFRecord]
findRecordsByDN (LDIFContent _ entries) dn = filter (\x -> (reDN x) == dn) entries
findRecordsByDN (LDIFChanges _ entries) dn = filter (\x -> (reDN x) == dn) entries

-- | Find first Content with given DN
findRecordByDN :: LDIF -> DN -> Maybe LDIFRecord
findRecordByDN ldif dn = case findRecordsByDN ldif dn of
                                 []   -> Nothing
                                 xs   -> Just (head xs)


-- | Change record without any impact
isDummyRecord :: LDIFRecord -> Bool
isDummyRecord (ChangeRecord _ (ChangeModify [])) = True 
isDummyRecord _ = False

leafOfDN :: DN -> AttrValue
leafOfDN xs = getDNValue xs 0

rootOfDN :: DN -> AttrValue
rootOfDN xs = getDNValue xs ((sizeOfDN xs)-1)

sizeOfDN :: DN -> Int
sizeOfDN (DN vals) = length vals

getDNValue :: DN -> Int -> AttrValue
getDNValue (DN vals) idx = vals !! idx

takeDNPrefix :: DN -> Int -> DN
takeDNPrefix (DN vals) n = (DN (take n vals))

-- | Check if the dn1 is prefix of dn2
isDNPrefixOf :: DN -> DN -> Bool
isDNPrefixOf dn1 dn2 | (sizeOfDN dn1) >= (sizeOfDN dn2) = False
                     | otherwise = let n = (sizeOfDN dn2)
                                   in (takeDNPrefix dn1 n) == dn2

