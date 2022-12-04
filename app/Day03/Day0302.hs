module Main where

import qualified Data.Map as Map
import Data.List as List
import Data.List.Split (chunksOf)
import Data.Coerce
import Helpers
import Text.Parsec


data Group = Group Rucksack Rucksack Rucksack deriving (Show)

newtype Rucksack = Rucksack [Item] deriving (Show)

newtype Item = Item Char deriving (Show, Eq, Ord)

itemTypes :: [Item]
itemTypes = coerce $ ['a'..'z'] ++ ['A'..'Z']

findBadge :: Group -> Item
findBadge (Group (Rucksack comp1) (Rucksack comp2) (Rucksack comp3)) = 
    List.head $ List.intersect comp1 $ List.intersect comp2 comp3

itemPriorities :: Map.Map Item Int
itemPriorities = Map.fromList $ zip itemTypes [1..52]

getPriority :: Item -> Int
getPriority item = itemPriorities Map.! item

rucksackParser :: Parsec String () Rucksack
rucksackParser = do
    items <- many (oneOf $ coerce itemTypes)
    endOfLine
    return $ Rucksack $ coerce items

groupParser :: Parsec String () Group
groupParser = do
    r1 <- rucksackParser
    r2 <- rucksackParser
    r3 <- rucksackParser
    return $ Group r1 r2 r3

fileParser :: Parsec String () [Group]
fileParser = do
    groups <- many groupParser
    eof
    return groups

main = do
    input <- parseFile fileParser "app/Day03/Input.txt"
    print $ fmap (sum . fmap (getPriority . findBadge)) input
     




