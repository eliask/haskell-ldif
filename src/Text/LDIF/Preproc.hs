{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Text.LDIF.Preproc ( preproc
                         , transposePos
                         , PosTable )
where
import Text.Parsec
import Text.Parsec.Error (setErrorPos)
import qualified Data.ByteString.Char8 as BC
import Data.List (foldl', sortBy)

-- | Opaque data necessary for relation between text after preprocessing and original
type PosTable = [ PosOp ]

data PosOp = PosOpAddLine { psLine :: Int }
           | PosOpWrap    { psLine :: Int, psW :: Int, psWP :: Int } deriving Show

data LdifLine = LdifLine     { llNum :: Int, llStr :: BC.ByteString }
              | LdifLineCont { llNum :: Int, llStr :: BC.ByteString }
              | LdifComment  { llNum :: Int, llStr :: BC.ByteString }

-- | Convert error position to original text before preprocessing
transposePos :: PosTable -> ParseError -> ParseError
transposePos ptab oe = setErrorPos npos oe
  where
    opos = errorPos oe
    npos = setSourceColumn (setSourceLine opos nlin) ncol
      where
        opIdx a b = psLine a `compare` psLine b
        ocord = (sourceLine opos,sourceColumn opos)
        (nlin,ncol) = calcPos (sortBy opIdx ptab) ocord

calcPos :: PosTable -> (Int, Int) -> (Int, Int)
calcPos xs cord = foldl' updatePos cord xs
  where
    updatePos (l0,c0) (PosOpAddLine l)    | l0 >= l        = (l0+1,c0)
    updatePos (l0,c0) (PosOpWrap l _ _)  | l0 >= l        = (l0+1,c0)
    updatePos (l0,c0) (PosOpWrap l w wp)  | (l0+1) == l && c0 > w && (c0-1-w) > wp  = (l0+1,c0)
    updatePos (l0,c0) (PosOpWrap l w wp)  | (l0+1) == l && c0 > w && (c0-1-w) <= wp = (l0+1,c0-w)
    updatePos x _ = x

-- | Preprocessing of LDIF file, concat wrapped lines and remove comment lines
preproc :: BC.ByteString -> (BC.ByteString, PosTable)
preproc xs = (str, ptab)
  where
    str = BC.unlines $ map llStr ys
    (ys, ptab) = lns xs
      where
        lns zs = stripComments $ unwrap (tokenizeLines $ specLines zs, [])

specLines :: BC.ByteString -> [BC.ByteString]
specLines xs = map cleanLine $ BC.lines xs
  where
    isCR c = c == '\r'
    cleanLine l | BC.null l        = l
                | isCR (BC.last l) = cleanLine $ BC.init l
                | otherwise        = l

tokenizeLines :: [BC.ByteString] -> [LdifLine]
tokenizeLines xs = zipWith (curry tokenizeLine) xs [1..]
  where
    tokenizeLine (x,i) | BC.null x        = LdifLine     i BC.empty
                       | BC.head x == '#' = LdifComment  i x
                       | BC.head x == ' ' = LdifLineCont i $ BC.tail x
                       | otherwise        = LdifLine     i x

-- | Remove Comment Lines
stripComments :: ([LdifLine],PosTable) -> ([LdifLine],PosTable)
stripComments (xs,pt) = foldl' procLine ([],pt) xs
  where
    procLine (v,p) (LdifComment i _) = (v, PosOpAddLine i : p)
    procLine (v,p) o                 = (o : v, p)

-- | Unwrap lines, lines with space at begin is continue of previous line
unwrap :: ([LdifLine],PosTable) -> ([LdifLine],PosTable)
unwrap (xs,pt) = foldl' procLine ([],pt) xs
  where
    procLine ([],p) o = ([o], p)
    procLine (v,p) (LdifLineCont i s) = let (z,r) = splitAt 1 v
                                            o = head z
                                            o' = o { llStr = llStr o `BC.append` s }
                                            p' = PosOpWrap i (BC.length $ llStr o) (BC.length s) : p
                                        in (o':r,p')
    procLine (v,p) o                  = (o:v,p)
