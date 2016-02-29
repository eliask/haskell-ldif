-- | LDIF related operations
module Text.LDIF.Proc (
        findChangesByDN,
        findContentsByDN,
	findContentByDN,
	diffLDIF,
        diffRecord 
)
where
import Text.LDIF.Types
import Data.Maybe

-- | Find all Changes with given DN
findChangesByDN :: LDIF -> DN -> [ChangeRecord]
findChangesByDN _ _ = error "not implemented"

-- | Find all Contents with given DN
findContentsByDN :: LDIF -> DN -> [ContentRecord]
findContentsByDN (LDIFContent _ entries) dn = filter (\x -> (coDN x) == dn) entries
findContentsByDN _ _ = []

-- | Find first Content with given DN
findContentByDN :: LDIF -> DN -> Maybe ContentRecord
findContentByDN ldif dn = case findContentsByDN ldif dn of
                                 []   -> Nothing
                                 xs   -> Just (head xs)

-- | Create Change LDIF between to LDIF contents. If any
-- | of input argument is not LDIFContent it returns Nothing. 
-- | If there is not difference the Change LDIF with empty
-- | change list is returned.
-- |
-- | Unsing following strategy: 
-- | 1. Iterate over L1 DN's and Modify / Remove Content 
-- | 2. Iterate over L2 and Add Content not in L1
diffLDIF :: LDIF -> LDIF -> Maybe LDIF
diffLDIF (LDIFContent _ c1) l2@(LDIFContent v2 _) = Just (LDIFChanges v2 changes) 
   where 
      changes = filter (not . isDummyChangeRecord) $ foldl (processEntry) [] c1
      processEntry xs e1 = let me2 = findContentByDN l2 (coDN e1) 
                               change = case me2 of
					   Nothing -> ChangeRecord (coDN e1) ChangeDelete
                                           Just e2 -> fromJust $ diffRecord e1 e2
                           in xs ++ [change]
diffLDIF _ _ = Nothing

-- | Diff two AttrVal Records if any of provided. 
-- | Implementation uses inefficient algorithm for large count of attributes within ContentRecord.
diffRecord :: ContentRecord -> ContentRecord -> Maybe ChangeRecord
diffRecord r1 r2 | (coDN r1) /= (coDN r2) = Nothing
                 | otherwise = Just (ChangeRecord (coDN r1) (ChangeModify mods))
   where
      mods = delMods ++ addMods
      addMods = map (\x -> ModAdd (fst x) [(snd x)]) addVals
      delMods = map (\x -> ModDelete (fst x) [(snd x)]) delVals
      addVals = filter (\x -> not $ elem x (coAttrVals r1)) (coAttrVals r2) :: [AttrValue]
      delVals = filter (\x -> not $ elem x (coAttrVals r2)) (coAttrVals r1) :: [AttrValue]

-- | Change record without any impact
isDummyChangeRecord :: ChangeRecord -> Bool
isDummyChangeRecord (ChangeRecord _ (ChangeModify [])) = True 
isDummyChangeRecord _ = False
