module Text.LDIF.Diff (
        diffLDIF,
        diffRecord,
        compareLDIF
)
where
import Text.LDIF.Types
import Text.LDIF.Utils
import Data.Maybe
import Data.List (foldl', sortBy)

-- | Calculate Change LDIF between two LDIF contents.
-- If there is not difference the empty change list is returned.
diffLDIF :: LDIF -> LDIF -> Either String LDIF
diffLDIF l1 l2 | getLDIFType l1 == LDIFContentType && getLDIFType l2 == LDIFContentType = Right (diffLDIF' l1 l2)
               | otherwise = Left ("Diff supported only on Content LDIFs but SRC="
                                         ++(show $ getLDIFType l1)++" and DST="
                                         ++(show $ getLDIFType l2))

diffLDIF' :: LDIF -> LDIF -> LDIF
diffLDIF' l1@(LDIF _ c1) l2@(LDIF v2 c2) = LDIF v2 (changes ++ deletes ++ adds)
   where 
      adds = map content2add $ filter (not . isEntryIn) c2
        where
          llc = createLookupTable l1
          isEntryIn ex = case findRecordByDN llc (reDN ex) of
            Nothing                   -> False
            Just (ContentRecord _ _)  -> True
            Just (ChangeRecord _ _)   -> error "Unexpected record type"
          content2add (ContentRecord dn vals) = ChangeRecord dn (ChangeAdd vals)
          content2add (ChangeRecord _ _)      = error "Unexpected record type"
      (changes,deletes) = (reverse $ fnu changes', fnu deletes')
        where
          fnu = filter (not . isDummyRecord)
          l2c = createLookupTable l2
          (changes',deletes') = foldl' processEntry ([],[]) c1
          processEntry (cx,dx) e1 = procEntry' $ findRecordByDN l2c (reDN e1)
            where
              procEntry' Nothing   = (cx, (ChangeRecord (reDN e1) ChangeDelete):dx)
              procEntry' (Just e2) = ((fromJust $ diffRecord e1 e2):cx, dx)

-- | Calculate difference between two LDIF Records
diffRecord :: LDIFRecord -> LDIFRecord -> Maybe LDIFRecord
diffRecord r1 r2 | (reDN r1) /= (reDN r2) = Nothing
                 | otherwise = Just (ChangeRecord (reDN r1) (ChangeModify mods))
   where
      mods = delMods ++ addMods
      addMods = map (\x -> ModAdd (fst x) [(snd x)]) addVals
      delMods = map (\x -> ModDelete (fst x) [(snd x)]) delVals
      addVals = filter (\x -> not $ elem x (coAttrVals r1)) (coAttrVals r2) :: [AttrValue]
      delVals = filter (\x -> not $ elem x (coAttrVals r2)) (coAttrVals r1) :: [AttrValue]

-- | Compare two LDIFs and provide list of the Records for each input LDIF,
-- which are different or not present in the other LDIF.
compareLDIF :: LDIF -> LDIF -> ([LDIFRecord], [LDIFRecord])
compareLDIF l1@(LDIF _ c1) l2@(LDIF _ c2) = (sortBy cmpByDN r1, sortBy cmpByDN $ r2 ++ adds)
   where 
      cmpByDN a b = (reDN a) `compare` (reDN b)
      adds = filter (not . isEntryIn) c2
        where
          llc = createLookupTable l1
          isEntryIn ex = case findRecordByDN llc (reDN ex) of
            Nothing                   -> False
            (Just _)                  -> True
      (r1,r2) = foldl' processEntry ([],[]) c1
        where
          l2c = createLookupTable l2
          processEntry (a1,a2) e1 = case findRecordByDN l2c (reDN e1) of
            Nothing -> (e1:a1,a2)
            Just e2 -> if e1 == e2 then (a1,a2) else (e1:a1,e2:a2)
