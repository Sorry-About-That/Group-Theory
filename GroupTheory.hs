module GroupTheory where

import Prelude hiding (Word)
import Data.List (transpose)

import Util (elemIndex)


type Elem  = Int       -- group element
type Word  = [Elem]    -- word is any written product of group elements and their inverses
type Table = [[Elem]]  -- this will be the Caley table (without the headers)


data Caley = Caley [Elem] Table

data Group = Group Caley String


groupOperation :: Group -> Elem -> Elem -> Elem
groupOperation (Group caley _) a b = (table !! i) !! j
  where
    Caley elems table              = caley
    (i, j)                         = (a `elemIndex` elems, b `elemIndex` elems)

toCaley :: Group -> Caley
toCaley (Group caley _) = caley

toTable :: Group -> Table
toTable group           = table
  where Caley _ table   = toCaley group

toElems :: Group -> [Elem]
toElems group           = elems
  where Caley elems _   = toCaley group

identity :: Group -> Elem
identity (Group caley _) = elems !! i
  where
    Caley elems table    = caley
    i                    = elems `elemIndex` table

isElem :: Word -> Group -> Bool
isElem word group    = null invalidChars
  where invalidChars = filter (`notElem` toElems group) word

reduce :: Word -> Group -> Elem
reduce []   group = identity group
reduce word group = foldr1 f word
  where f         = groupOperation group

order :: Elem -> Group -> Int
order a group = length (takeWhile (/= identity group) (iterate (f a) a)) + 1
  where f     = groupOperation group

cardinality :: Group -> Int
cardinality = length . toElems


isAbelian :: Group -> Bool
isAbelian group = transpose table == table
  where table   = toTable group

isCyclic :: Group -> Bool
isCyclic group = length elems `elem` map (`order` group) elems
  where elems  = toElems group
