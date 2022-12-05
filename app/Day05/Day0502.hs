{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Text.Parsec hiding (space)
import Helpers
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe

data CrateInput = CrateInput Char | NoCrate deriving (Show)
type Crate = Char
type Stack = [Crate]
type Supplies = Map.Map Int Stack
data Move = Move { amountOfCrates :: Int, fromStack :: Int, toStack :: Int } deriving (Show)

makeStack :: [CrateInput] -> Stack
makeStack crateInputs = 
    let toCrate crateInput crates =
             case crateInput of
                    CrateInput crate -> crate : crates
                    NoCrate -> crates
        
        crates = foldr toCrate [] crateInputs
    in crates

makeMove :: Supplies -> Move -> Supplies
makeMove supplies (Move {amountOfCrates, fromStack, toStack }) = fromMaybe supplies $ do
        from <- Map.lookup fromStack supplies
        to <- Map.lookup toStack supplies
        let (crate, newFrom) = splitAt amountOfCrates from
        let newTo = crate ++ to
        let newSupplies = Map.insert toStack newTo $ Map.insert fromStack newFrom $ supplies
        return newSupplies

runCrane :: (Supplies, [Move]) -> Supplies
runCrane (supplies, moves) = foldl makeMove supplies moves

collectResults :: Supplies -> [Crate]
collectResults supplies = fmap head $ Map.elems supplies

main = do
    input <- parseFile fileParser "app/Day05/Input.txt"
    print $ fmap (collectResults . runCrane) input 

-- Parsing

space = char ' '

crateInputParser :: Parsec String () CrateInput
crateInputParser = do
    char '['
    crate <- letter
    char ']'
    return $ CrateInput crate

noCrateParser :: Parsec String () CrateInput
noCrateParser = do
    space
    space
    space
    return NoCrate

crateLineParser :: Parsec String () [CrateInput]
crateLineParser = do
    crateInputs <- (crateInputParser <|> noCrateParser) `sepBy` space
    endOfLine
    return crateInputs

stackIndexParser :: Parsec String () Int
stackIndexParser = do
    space
    index <- read <$> many digit
    space
    return index

stackIndexLineParser :: Parsec String () [Int]
stackIndexLineParser = do
    stackIndices <- stackIndexParser `sepBy` space
    endOfLine
    return stackIndices

stacksParser :: Parsec String () Supplies
stacksParser = do
    crateLines <- manyTill crateLineParser (try stackIndexLineParser)
    return $ Map.fromList $ zip [1..] $ makeStack <$> transpose crateLines

moveParser :: Parsec String () Move
moveParser = do
    string "move "
    amountOfCrates <- read <$> many digit
    string " from "
    fromStack <- read <$> many digit
    string " to "
    toStack <- read <$> many digit
    return $ Move amountOfCrates fromStack toStack

fileParser :: Parsec String () (Supplies, [Move])
fileParser = do
    stacks <- stacksParser
    endOfLine
    moves <- moveParser `sepBy` endOfLine
    eof
    return (stacks, moves)
