{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- | LDIF representation in Data.Tree structure
module Text.LDIF.Tree ( toTree, fromTree, sortTreeByName )
       where
import Prelude
import Text.LDIF.Types
import Text.LDIF.Utils
import Data.Tree
import Data.Maybe
import Data.List
import qualified Data.Set as S
import qualified Data.Tree.Zipper as Z

-- | Flatten Tree of Records to LDIF
fromTree :: Tree LDIFRecord -> LDIF
fromTree !xs = ys `seq` LDIF Nothing ys
  where
    ys = (filter (not . isFakeEntry) $ flatten xs)
      where
        isFakeEntry (ContentRecord _ []) = True
        isFakeEntry _ = False

-- | Convert LDIF to Tree of Records using their DNs. Can insert dummy parents.
toTree :: LDIF -> Bool -> Tree LDIFRecord
toTree (LDIF _ xs) False = fromRecords xs
toTree (LDIF _ xs) True  = fromRecords $ addFakeParents xs
        
addFakeParents :: [ LDIFRecord ] -> [ LDIFRecord ]        
addFakeParents entries = fakeParents ++ entries
  where 
    fakeParents = map fakeParent missingDNs
      where
        fakeParent dn = ContentRecord dn []
        missingDNs = filter ((flip S.notMember) allDNs) $ S.toList parentDNs
          where
            allDNs = S.fromList $ map reDN entries
            parentDNs =  S.fromList $ map DN $ filter (not . null) $ concatMap (tails . dnAttrVals) $ S.toList allDNs
    
rootEntry :: Tree LDIFRecord    
rootEntry = Node (ContentRecord (DN []) []) []

fromRecords :: [LDIFRecord] -> Tree LDIFRecord
fromRecords xs = Z.toTree $ foldl' addEntry (Z.fromTree rootEntry) $ sortBy compareByDNLen xs
      where
        compareByDNLen a b = (lengthOfDN $ reDN a) `compare` (lengthOfDN $ reDN b)
        addEntry tree entry = Z.root $ Z.insert (Node entry []) $ findParent tree
          where
            findParent z | not $ Z.hasChildren z = Z.children z -- No children; put it here
                         | isNothing child       = Z.children z -- No matching child; put it here
                         | otherwise             = findParent $ fromJust child -- found matching child, continue
              where
                child = findChild $ Z.firstChild z -- Traverse all childs
                  where
                    findChild Nothing  = Nothing -- Nothing found
                    findChild (Just c)  | (Z.label c) `isParentRecordOf` entry = Just c  -- Found
                                        | otherwise                            = findChild $ Z.next c -- Continue

-- | Sort recursively children Records by DNs
sortTreeByName :: Tree LDIFRecord -> Tree LDIFRecord
sortTreeByName (Node n []) = Node n []
sortTreeByName (Node n xs) = let ys = sortBy cmpDN xs
                                 cmpDN a b = (reDN $ rootLabel a) `compare` (reDN $ rootLabel b)
                             in Node n (map sortTreeByName ys)
