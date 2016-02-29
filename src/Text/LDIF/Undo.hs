{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Text.LDIF.Undo (
        undoLDIF
)
where
import Data.Maybe
import Text.LDIF.Types

-- | Warning message when undo can not be calculated
type Warning = String

-- | Calculate undo LDIF
undoLDIF :: LDIF -> (LDIF,[[Warning]])
undoLDIF (LDIF v xs) = let (ys,w) = let zs = map undoRecord $ reverse xs
                                    in (map fst zs, map snd zs)
                       in (LDIF v ys, filter (not . null) w)

-- | Calculate undo Record with possible warnings
undoRecord :: LDIFRecord -> (LDIFRecord,[Warning])
undoRecord (ContentRecord dn _) = (ChangeRecord dn ChangeDelete,[])
undoRecord (ChangeRecord  dn (ChangeAdd _))     = (ChangeRecord dn ChangeDelete,[])
undoRecord (ChangeRecord  dn ChangeDelete)      = (ChangeRecord dn (ChangeAdd []), [wrnO dn "delete"])
undoRecord (ChangeRecord  dn (ChangeModify xs)) = let (x, w) = let ys = map undoMod $ reverse xs
                                                               in (map fst ys, map snd ys)
                                                  in (ChangeRecord dn (ChangeModify x), catMaybes w)
undoRecord x = (x, ["Unsupported operation"])

-- | Calculate undo Modification with possible warning
undoMod :: Modify -> (Modify,Maybe Warning)
undoMod (ModAdd a v)     = (ModDelete a v, Nothing)
undoMod (ModDelete a [])  = (ModAdd a [], Just $ wrnA a "delete")
undoMod (ModDelete a zs)  = (ModAdd a zs, Nothing)
undoMod (ModReplace a _) = (ModReplace a [], Just $ wrnA a "replace")

wrnA :: Attribute -> String -> String
wrnA a op = concat [show $ aName a, " ", op]

wrnO :: DN -> String -> String
wrnO dn op = concat [show dn, " ", op]
