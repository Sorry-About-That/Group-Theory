module Group where
import Data.List (transpose)
import Prelude hiding (Word)


data Caley = Caley String Table

data Group = Group Caley String

type Word  = String   -- word is any written product of group elements and their inverses

type Table = [[Char]] -- this will be the Caley table (without the headers)

groupOperation :: Group -> Char -> Char -> Char
groupOperation (Group caley _) a b   = (table !! i) !! j
    where
        Caley elems table            = caley
        (i,j)                        = (a `elemIndex` elems, b `elemIndex` elems)


elemIndex :: Eq a => a -> [a] -> Int
x `elemIndex` (y:ys)
    | x == y    = 0
    | otherwise = (x `elemIndex` ys) + 1


toCaley :: Group -> Caley
toCaley (Group caley _) = caley

toTable :: Group -> Table
toTable group           = table
    where Caley _ table = toCaley group

toElems :: Group -> String
toElems group           = elems
    where Caley elems _ = toCaley group

isAbelian :: Group -> Bool
isAbelian (Group caley _) = transpose table == table
    where Caley _ table   = caley

kleinFourTable :: Caley
kleinFourTable = Caley "eabc" ["eabc", "aecb", "bcea", "cbae"]

kleinFour :: Group
kleinFour = Group kleinFourTable "V4"

identity :: Group -> Char
identity (Group caley _)  = elems !! idx
    where
        Caley elems table = caley
        idx               = elems `elemIndex` table

isElem :: Word -> Group -> Bool
isElem word grp        = null invalidChars
    where invalidChars = filter (`notElem` toElems grp) word

reduce :: Word -> Group -> Char
reduce ""   _   = ' '
reduce word grp = foldr1 f word
    where f     = groupOperation grp

order :: Char -> Group -> Int
order a grp = length (takeWhile (/= identity grp) (iterate (f a) a)) + 1
    where f = groupOperation grp

cardinality :: Group -> Int 
cardinality grp = length $ toElems grp

isCyclic :: Group -> Bool
isCyclic grp  = length elems `elem` map (`order` grp) elems
    where
        elems = toElems grp




