{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- | LDIF related types
module Text.LDIF.Types (
 	LDIF(..),   
        LDIFRecord(..),
        Change(..),
        Modify(..), 
        DN(..), 
        LDIFType(..),
        Attribute(..), Value(..), AttrValue,
        isContentRecord,
        isChangeRecord,
        getLDIFType
)
where
import qualified Data.ByteString.Char8 as BC
import Data.Char

-- | Attribute name is case-insensitive string
data Attribute = Attribute { aName :: BC.ByteString } deriving Show

instance Eq Attribute where
    (Attribute xs) == (Attribute ys)  = (BC.map toUpper xs) == (BC.map toUpper ys)

instance Ord Attribute where
    (Attribute xs) `compare` (Attribute ys)  = (BC.map toUpper xs) `compare` (BC.map toUpper ys)

-- | Attribute value is either case sensitive or insensitive string
data Value = Value  { aVal :: BC.ByteString }
           | ValueI { aVal :: BC.ByteString } deriving Show
             
instance Eq Value where
    (Value xs) == (Value ys)  = xs == ys
    xs == ys  = (BC.map toUpper $ aVal xs) == (BC.map toUpper $ aVal ys)

instance Ord Value where
    (Value xs) `compare` (Value ys)  = xs `compare` ys
    xs `compare` ys = (BC.map toUpper $ aVal xs) `compare` (BC.map toUpper $ aVal ys)

-- | Pair of Atribute and Value
type AttrValue = (Attribute, Value)

-- | Enumeration LDIF Types
data LDIFType = LDIFContentType -- ^ LDIF with Content Records
              | LDIFChangesType -- ^ LDIF with Changes Records
              | LDIFMixedType   -- ^ LDIF with both Content and Changes Records
              deriving Eq

instance Show LDIFType where
    show LDIFChangesType = "Delta"
    show LDIFContentType = "Content"
    show LDIFMixedType   = "Mixed"

-- | Represents LDIF structure, it can be either simply LDIF data dump or
-- changes LDIF with LDAP operations 
data LDIF = LDIF { lcVersion :: Maybe BC.ByteString, lcEntries :: ![LDIFRecord] } deriving (Show, Eq)

data LDIFRecord
  -- | Represents one data record within LDIF file with DN and content
  = ContentRecord { reDN :: !DN, coAttrVals :: ![AttrValue] } 
  -- | Represents one change record within LDIF file with DN and content
  | ChangeRecord  { reDN :: !DN, chOp :: !Change } deriving (Show, Eq)

-- | Represents one LDAP operation within changes LDIF
data Change = ChangeAdd     { chAttrVals :: ![AttrValue] }
            | ChangeDelete 
            | ChangeModify  { chMods :: ![Modify] }
            | ChangeModDN  deriving (Show, Eq)

-- | Represents ChangeModify operations upon one entry within given DN
data Modify = ModAdd     { modAttr :: !Attribute, modAttrVals :: ![Value] }
            | ModDelete  { modAttr :: !Attribute, modAttrVals :: ![Value] }
            | ModReplace { modAttr :: !Attribute, modAttrVals :: ![Value] } deriving (Show, Eq)

-- | Represents Distinguished Name (DN)
data DN = DN { dnAttrVals :: ![AttrValue] } deriving (Eq, Show)

-- | Ord Instance for DN
instance Ord DN where
  (DN xs1) `compare` (DN xs2)  = let cmpAV ((a1,v1),(a2,v2)) = let ca = a1 `compare` a2
                                                                   cv = v1 `compare` v2
                                                               in if ca == EQ then cv else ca
                                     dx = map cmpAV $ zip (reverse $ xs1) (reverse $ xs2)
                                     lx | length xs1 > length xs2 = GT
                                        | length xs1 < length xs2 = LT
                                        | otherwise = EQ
                                 in case filter (EQ /=) dx of
                                   []    -> lx
                                   (x:_) -> x

-- | Check if LDIFRecord is Content Record
isContentRecord :: LDIFRecord -> Bool
isContentRecord (ContentRecord _ _) = True
isContentRecord _ = False

-- | Check if LDIFRecord is Change Record
isChangeRecord :: LDIFRecord -> Bool
isChangeRecord (ChangeRecord _ _) = True
isChangeRecord _ = False

-- | Dettect from LDIF content the Type (Content, Changes, Mixed)
getLDIFType :: LDIF -> LDIFType
getLDIFType (LDIF _ []) = LDIFContentType
getLDIFType (LDIF _ xs) = getLDIFType' con chg
    where
      con = filter (isContentRecord) xs
      chg = filter (not . isContentRecord) xs
      getLDIFType' [] [] = LDIFContentType -- Fallback Empty LDIF as an Content LDIF
      getLDIFType' [] _  = LDIFChangesType
      getLDIFType' _  [] = LDIFContentType
      getLDIFType' _  _  = LDIFMixedType
