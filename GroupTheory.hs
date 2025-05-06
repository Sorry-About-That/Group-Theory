module GroupTheory where

import Prelude hiding (Word)
import Data.List (transpose)

import Util (elemIndex)


type Elem  = Int       -- group element
type Word  = [Elem]    -- word is any written product of group elements and their inverses
type Table = [[Elem]]  -- this will be the Caley table (without the headers)


data Group = Group Table String


groupOperation :: Group -> Elem -> Elem -> Elem
groupOperation group a b = (table !! i) !! j
  where
    (elems, table)       = (toElems group, toTable group)
    (i, j)               = (a `elemIndex` elems, b `elemIndex` elems)

toElems :: Group -> [Elem]
toElems group = [0..len]
  where len   = length (toTable group) - 1

toTable :: Group -> Table
toTable (Group table _) = table

identity :: Group -> Elem
identity group     = elems !! i
  where
    (elems, table) = (toElems group, toTable group)
    i              = elems `elemIndex` table

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
cardinality = length . toTable


isAbelian :: Group -> Bool
isAbelian (Group table _) = transpose table == table

isCyclic :: Group -> Bool
isCyclic group = length elems `elem` map (`order` group) elems
  where elems  = toElems group
