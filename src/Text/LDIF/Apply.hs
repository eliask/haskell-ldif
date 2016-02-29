module Text.LDIF.Apply (
        applyLDIF
)
where
import Text.LDIF.Types
import Text.LDIF.Utils
import Text.LDIF.Printer
import Data.List (nub, foldl')

-- | Apply one LDIF to another LDIF. The destination LDIF has
-- | to be Content LDIF
applyLDIF :: LDIF -> LDIF -> LDIF
applyLDIF dst (LDIF _ xs) = foldl' (\ld chg -> applyRecord2LDIF chg ld) dst xs

-- | Apply one LDIF Content/Change Record into LDIF and produce Changed LDIF
applyRecord2LDIF :: LDIFRecord -> LDIF -> LDIF
applyRecord2LDIF (ContentRecord dn vals) dst = applyRecord2LDIF (ChangeRecord dn (ChangeAdd vals)) dst
applyRecord2LDIF (ChangeRecord  dn op)   dst = applyChange2Record op dn dst (findRecordByDN dst dn)

-- | Apply one LDIF Change (add/del/modf) for given DN within LDIF Content 
applyChange2Record :: Change -> DN -> LDIF -> Maybe LDIFRecord -> LDIF
applyChange2Record (ChangeAdd vals)   dn ld Nothing  = LDIF (lcVersion ld) ((lcEntries ld)++[ContentRecord dn vals]) 
applyChange2Record (ChangeAdd _)      dn _  (Just _) = error ("ADD: Already exists: "++(dn2str dn))
applyChange2Record ChangeDelete       dn ld (Just _) = LDIF (lcVersion ld) (filter (\x -> dn /= (reDN x)) (lcEntries ld))
applyChange2Record ChangeDelete       dn _  Nothing  = error ("DELETE: Entry not found: "++(dn2str dn))
applyChange2Record (ChangeModify _)   dn _  Nothing  = error ("MODIFY: Entry not found: "++(dn2str dn))
applyChange2Record (ChangeModify ops) dn ld (Just r) = let pre  = takeWhile (\x -> dn /= (reDN x)) (lcEntries ld)
                                                           post = filter (\x -> dn /= (reDN x)) $ dropWhile (\x -> dn /= (reDN x)) (lcEntries ld)
                                                           rn   = foldr applyMod2Record r ops
                                                       in LDIF (lcVersion ld) (pre++[rn]++post)
applyChange2Record ChangeModDN      _ _ _  = error "ModDN: Operation is not supported yet."
--applyChange2Record x _ y _                 = error $ "Unexpected LDIF Content: " ++ (show x) ++ (show y)

-- | Apply Attribute Modification (Add/Del/Replace) to ContentRecord and produce changed ContentRecord
applyMod2Record :: Modify -> LDIFRecord -> LDIFRecord
applyMod2Record (ModAdd      name vals) (ContentRecord dn av) = let nav = map (\v -> (name,v)) vals  -- new attr/values
                                                                    mav = av ++ nav                  -- merged attr/values
                                                                    verified = if (length $ nub mav) == (length mav) then mav
                                                                               else error ("ModAdd: Values already exists: "++(show av)++" vs "++(show vals)++" DN:"++(dn2str dn))
                                                                    in ContentRecord dn verified
applyMod2Record (ModDelete   name [])   (ContentRecord dn av) = let nav = filter (\(n,_) -> n /= name) av -- new attr/values
                                                                    verified = if (length $ nub nav) /= (length av) then nav
                                                                               else error ("ModDel: Attribute not found: "++(show name)++" DN:"++(dn2str dn))
                                                                in ContentRecord dn verified
applyMod2Record (ModDelete   name vals) (ContentRecord dn av) = let mav = filter (\(n,v) -> n /= name || v `notElem` vals) av
                                                                    verified = if (length av) - (length mav) == (length vals) then mav
                                                                               else error ("ModDel: Attribute/Value not found: "++(show name)++"vals"++(show vals)++" DN:"++(dn2str dn))
                                                                in ContentRecord dn verified
applyMod2Record (ModReplace  name vals) (ContentRecord dn av) = let nav = map (\v -> (name,v)) vals -- new attr/values
                                                                    mav = (filter (\(n,_) -> n /= name) av) ++ nav -- merged
                                                                in ContentRecord dn mav
applyMod2Record _ x = error $ "Unexpected LDIF Record:" ++ (show x)
