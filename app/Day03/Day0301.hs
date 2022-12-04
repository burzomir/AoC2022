module Main where

import qualified Data.Map as Map
import Data.List as List
import Data.Coerce

data Rucksack = Rucksack Compartment Compartment deriving (Show)

newtype Compartment = Compartment [Item] deriving (Show)

newtype Item = Item Char deriving (Show, Eq, Ord)

itemTypes :: [Item]
itemTypes = coerce $ ['a'..'z'] ++ ['A'..'Z']

parseRucksack :: String -> Rucksack
parseRucksack items = 
    let (comp1, comp2) = splitAt (length items `div` 2) (coerce items)
    in Rucksack (Compartment comp1) (Compartment comp2)

findError :: Rucksack -> Item
findError (Rucksack (Compartment comp1) (Compartment comp2)) = 
    List.head $ List.intersect comp1 comp2

itemPriorities :: Map.Map Item Int
itemPriorities = Map.fromList $ zip itemTypes [1..52]

getPriority :: Item -> Int
getPriority item = itemPriorities Map.! item

lineToPriority :: String -> Int
lineToPriority line = getPriority $ findError $ parseRucksack line

main = do
    input <- readFile "app/Day03/Input.txt"
    let sumOfPriorities = sum $ fmap lineToPriority $ lines input
    print sumOfPriorities
     




