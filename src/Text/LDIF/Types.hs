-- | LDIF related types
module Text.LDIF.Types (
 	LDIF(..),   
        ContentRecord(..),
        ChangeRecord(..),
        Change(..),
        Modify(..), 
        DN(..), 
        LDIFType(..),
        Attribute, Value, AttrValue
)
where

type Attribute = String
type Value = String
type AttrValue = (Attribute, Value)

-- | Type of LDIF Files (Content, Changes)
data LDIFType = LDIFContentType | LDIFChangesType deriving (Show, Eq) -- Maybe LDIFMixedType 

-- | Represents LDIF structure, it can be either simply LDIF data dump or
-- | changes LDIF with LDAP operations 
data LDIF = LDIFContent { lcVersion :: Maybe String, lcEntries :: [ContentRecord] }
          | LDIFChanges { lcVersion :: Maybe String, lcChanges :: [ChangeRecord] } deriving (Show, Eq)

-- | Represents one data record within LDIF file with DN and content
data ContentRecord =ContentRecord { coDN :: DN, coAttrVals :: [AttrValue] } deriving (Show, Eq)

-- | Represents one change record within LDIF file with DN and content
data ChangeRecord = ChangeRecord  { chDN :: DN, chOp :: Change } deriving (Show, Eq)

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
data DN = DN { dnAttrVals :: [AttrValue] } deriving (Show, Eq)


