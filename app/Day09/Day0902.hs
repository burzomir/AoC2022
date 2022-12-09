module Main where

import Prelude hiding (Left, Right, init, tail)
import Text.Parsec
import Helpers
import Data.Either (fromRight)
import qualified Data.Set as Set
import Debug.Trace (trace)

data Position = Position { getX :: Int, getY :: Int } deriving (Eq, Ord)

instance Show Position where
    show (Position x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

data Direction 
    = Up
    | Down
    | Left
    | Right
    deriving Show


moveSegment :: Position -> Position -> Position
moveSegment tail head = 
    let diffX = getX head - getX tail
        diffY = getY head - getY tail
    in
        if abs diffX > 1 || abs diffY > 1 then
            let
                newX = getX tail + diffX - (if abs diffY > abs diffX then 0 else signum diffX * 1)
                newY = getY tail + diffY - (if abs diffX > abs diffY then 0 else signum diffY * 1)
            in Position newX newY
        else
            tail

moveHead :: Direction -> Position -> Position
moveHead Up (Position x y) = Position x (y - 1)
moveHead Down (Position x y) = Position x (y + 1)
moveHead Left (Position x y) = Position (x - 1) y
moveHead Right (Position x y) = Position (x + 1) y

moveTail :: Rope -> Rope
moveTail (h:[]) = h : []
moveTail (h:t:[]) = h : moveSegment t h : []
moveTail (h:t:rest) = h : (moveTail (moveSegment t h : rest))

type Rope = [Position]

rope :: Rope
rope = replicate 10 $ Position 0 0 

move :: Direction -> Rope -> Rope
move direction (head:tail) = 
    let newHead = moveHead direction head
    in moveTail (newHead : tail)

type Instruction = [Direction]

type Parser a = Parsec String () a

directionParser :: Parser Direction
directionParser = do
    direction <- oneOf "UDLR"
    return $ case direction of
        'U' -> Up
        'D' -> Down
        'L' -> Left
        'R' -> Right

repetitionsParser :: Parser Int
repetitionsParser = do
    read <$> many digit

instructionParser :: Parser Instruction
instructionParser = do
    direction <- directionParser
    char ' '
    repetitions <- repetitionsParser
    return $ replicate repetitions direction

inputParser :: Parser Instruction
inputParser = do
    instructions <- instructionParser `sepBy` endOfLine
    return $ concat instructions

solve =
    Set.size . Set.fromList . (fmap (head . reverse)) . scanl (flip move) rope

main = do
    instructions <- parseFile inputParser "app/Day09/Input.txt"
    print $ solve <$> instructions

