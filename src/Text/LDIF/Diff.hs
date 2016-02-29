module Text.LDIF.Diff (
        diffLDIF,
        diffRecord
)
where
import Text.LDIF.Types
import Text.LDIF.Utils
import Text.LDIF.Printer
import Data.Maybe
import Data.Either
import Data.List (nub)

-- | Create Change LDIF between to LDIF contents. If any
-- | of input argument is not LDIFContent it returns Nothing. 
-- | If there is not difference the Change LDIF with empty
-- | change list is returned.
-- |
-- | Unsing following strategy: 
-- | 1. Iterate over L1 DN's and Modify / Remove Content 
-- | 2. Iterate over L2 and Add Content not in L1
diffLDIF :: LDIF -> LDIF -> Maybe LDIF
diffLDIF l1@(LDIFContent _ c1) l2@(LDIFContent v2 c2) = Just (LDIFChanges v2 (changes ++ adds))
   where 
      adds = map (content2add) $ filter (not . isEntryIn l1) c2
      changes = filter (not . isDummyRecord) $ foldl (processEntry) [] c1
      processEntry xs e1 = let me2 = findRecordByDN l2 (reDN e1) 
                               change = case me2 of
					   Nothing -> ChangeRecord (reDN e1) ChangeDelete
                                           Just e2 -> fromJust $ diffRecord e1 e2
                           in xs ++ [change]
      isEntryIn ll ex = let mex = findRecordByDN ll (reDN ex)
                        in case mex of
                          Nothing -> False
                          Just _  -> True
      content2add (ContentRecord dn vals) = ChangeRecord dn (ChangeAdd vals)
diffLDIF _ _ = Nothing

-- | Diff two AttrVal Records if any of provided. 
-- | Implementation uses inefficient algorithm for large count of attributes within ContentRecord.
diffRecord :: LDIFRecord -> LDIFRecord -> Maybe LDIFRecord
diffRecord r1 r2 | (reDN r1) /= (reDN r2) = Nothing
                 | otherwise = Just (ChangeRecord (reDN r1) (ChangeModify mods))
   where
      mods = delMods ++ addMods
      addMods = map (\x -> ModAdd (fst x) [(snd x)]) addVals
      delMods = map (\x -> ModDelete (fst x) [(snd x)]) delVals
      addVals = filter (\x -> not $ elem x (coAttrVals r1)) (coAttrVals r2) :: [AttrValue]
      delVals = filter (\x -> not $ elem x (coAttrVals r2)) (coAttrVals r1) :: [AttrValue]
