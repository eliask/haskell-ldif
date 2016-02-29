-- | LDIF related types
module Text.LDIF.Types (
 	LDIF(..),   
        LDIFRecord(..),
        Change(..),
        Modify(..), 
        DN(..), 
        LDIFType(..),
        Attribute(..), Value, AttrValue
)
where
import Data.Char

newtype Attribute = Attribute String deriving Show

instance Eq Attribute where
    (Attribute xs) == (Attribute ys)  = (map toUpper xs) == (map toUpper ys)

instance Ord Attribute where
    (Attribute xs) `compare` (Attribute ys)  = (map toUpper xs) `compare` (map toUpper ys)

type Value = String
type AttrValue = (Attribute, Value)

-- | Type of LDIF Files (Content, Changes)
data LDIFType = LDIFContentType 
              | LDIFChangesType 
              | LDIFMixedType deriving Eq

instance Show LDIFType where
    show LDIFChangesType = "Delta"
    show LDIFContentType = "Content"
    show LDIFMixedType   = "Mixed"

-- | Represents LDIF structure, it can be either simply LDIF data dump or
-- | changes LDIF with LDAP operations 
data LDIF = LDIF { lcVersion :: Maybe String, lcEntries :: [LDIFRecord] } deriving (Show, Eq)

-- | Represents one data record within LDIF file with DN and content
-- | Represents one change record within LDIF file with DN and content
data LDIFRecord = ContentRecord { reDN :: DN, coAttrVals :: [AttrValue] } 
                | ChangeRecord  { reDN :: DN, chOp :: Change } deriving (Show, Eq)

-- | Represents one LDAP operation within changes LDIF
data Change = ChangeAdd     { chAttrVals :: [AttrValue] }
            | ChangeDelete 
            | ChangeModify  { chMods :: [Modify] }
            | ChangeModDN  deriving (Show, Eq)

-- | Represents ChangeModify operations upon one entry within given DN
data Modify = ModAdd     { modAttr :: Attribute, modAttrVals :: [Value] }
            | ModDelete  { modAttr :: Attribute, modAttrVals :: [Value] }
            | ModReplace { modAttr :: Attribute, modAttrVals :: [Value] } deriving (Show, Eq)

-- | Represents Distinguished Name (DN)
data DN = DN { dnAttrVals :: [AttrValue] } 
        | DNi { dnAttrVals :: [AttrValue] } deriving (Show)

instance Eq DN where
    (DN xs)  == (DN ys)   = xs == ys
    (DNi xs) == (DNi ys)  = (map (\(n,v) -> (n,(map toUpper v)))  xs) == (map (\(n,v) -> (n,(map toUpper v))) ys)
    x        == y         = (DNi (dnAttrVals x)) == (DNi (dnAttrVals y))
