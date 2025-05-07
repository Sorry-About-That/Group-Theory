module GroupTheory where

import Prelude hiding (Word)
import Data.List (transpose)

import Util (elemIndex)


type Elem  = Int       -- group element
type Word  = [Elem]    -- word is any written product of group elements and their inverses
type Table = [[Elem]]  -- this will be the Caley table (without the headers)


data Group = Group [Elem] Table String


toElems :: Group -> [Elem]
toElems (Group elems _ _) = elems

toTable :: Group -> Table
toTable (Group _ table _) = table

groupOperation :: Group -> Elem -> Elem -> Elem
groupOperation (Group elems table _) a b = (table !! i) !! j
  where (i, j) = (a `elemIndex` elems, b `elemIndex` elems)

identity :: Group -> Elem
identity (Group elems table _) = elems !! i
  where i = elems `elemIndex` table

isElem :: Word -> Group -> Bool
isElem word (Group elems _ _) = all (`elem` elems) word

reduce :: Word -> Group -> Elem
reduce []   group = identity group
reduce word group = foldr1 f word
  where f         = groupOperation group

order :: Elem -> Group -> Int
order a group = length (takeWhile (/= identity group) (iterate (f a) a)) + 1
  where f     = groupOperation group

cardinality :: Group -> Int
cardinality (Group elems _ _) = length elems

isAbelian :: Group -> Bool
isAbelian (Group _ table _) = transpose table == table

isCyclic :: Group -> Bool
isCyclic group@(Group elems _ _) = length elems `elem` map (`order` group) elems
