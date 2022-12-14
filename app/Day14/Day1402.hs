
module Main where

import Text.Parsec hiding ((<|>))
import File
import qualified Data.Map as Map
import Data.Either
import Control.Applicative
import Data.Maybe
import Prelude hiding (lookup)

type Position = (Int, Int)

newtype RockFormationData = RockFormationData [Position] deriving (Show)

type Parser a = Parsec String () a

positionParser :: Parser Position
positionParser = do
  x <- many1 digit
  _ <- char ','
  y <- many1 digit
  return (read x, read y)

rockFormationDataParser :: Parser RockFormationData
rockFormationDataParser = do
  positions <- positionParser `sepBy` string " -> "
  return $ RockFormationData positions

inputParser :: Parser [RockFormationData]
inputParser = rockFormationDataParser `sepBy` endOfLine

newtype RockFormation = RockFormation [Position] deriving (Show)

getRockPositions :: RockFormation -> [Position]
getRockPositions (RockFormation positions) = positions

makeRockFormation :: RockFormationData -> RockFormation
makeRockFormation (RockFormationData []) = RockFormation []
makeRockFormation (RockFormationData [p]) = RockFormation [p]
makeRockFormation (RockFormationData ((x1, y1):(x2, y2):ps)) = 
    let currentLine = [(x, y) | x <- [(min x1 x2)..(max x1 x2)], y <- [(min y1 y2)..(max y1 y2)]] 
        RockFormation nextLines = makeRockFormation $ RockFormationData ((x2, y2):ps) 
    in RockFormation $ currentLine ++ nextLines

getAllRockPositions :: [RockFormation] -> [Position]
getAllRockPositions rockFormations = concat $ fmap getRockPositions rockFormations

data Tile
    = Air
    | Rock
    | SandSource
    | SandGrain

instance Show Tile where
    show Air = "."
    show Rock = "#"
    show SandSource = "+"
    show SandGrain = "o"

type Height = Int
data Drawing = Drawing Height (Map.Map Position Tile) deriving (Show)

initDrawing :: [(Position, Tile)] -> Drawing
initDrawing tiles = 
    let height = (+2) $ maximum $ fmap (snd . fst) tiles
        drawing = Map.fromList tiles
    in Drawing height drawing

lookup :: Position -> Drawing -> Tile
lookup (x, y) (Drawing height drawing) = 
    if y >= height
        then Rock
        else case Map.lookup (x, y) drawing of
            Just tile -> tile
            Nothing -> Air

data SandGrainState
    = Falling
    | AtRest

dropSandGrain :: Position -> Drawing -> Position
dropSandGrain (x, y) drawing = 
    let fall = 
            fromMaybe (x, y) $ fallInto drawing (x - 1, y + 1) <|> fallInto drawing (x + 1, y + 1)
    in case lookup (x, y + 1) drawing of
        SandSource -> (x, y)
        Rock -> fall
        SandGrain -> fall
        Air -> dropSandGrain (x, y + 1) drawing

fallInto :: Drawing -> Position -> Maybe Position
fallInto drawing p = 
    if canFallInto p drawing
        then Just $ dropSandGrain p drawing
        else Nothing

canFallInto :: Position -> Drawing -> Bool
canFallInto (x, y) drawing = 
    case lookup (x, y) drawing of
        Rock -> False
        SandSource -> False
        SandGrain -> False
        Air -> True

solve :: (Int, Drawing) -> (Int, Drawing)
solve (totalSteps, drawing@(Drawing height d)) =
    let sandSource = (500, 0)
        position = dropSandGrain sandSource drawing
        newDrawing = Drawing height $ Map.insert position SandGrain d
        newTotalSteps = totalSteps + 1
    in if position == sandSource
        then (newTotalSteps, newDrawing)
        else solve (newTotalSteps, newDrawing)

render :: Position -> Position -> Drawing -> String
render (x1, y1) (x2, y2) drawing = 
    let rows = fmap (\ y -> fmap (\ x -> lookup (x, y) drawing) [x1..x2]) [y1..y2]
    in unlines $ fmap (concat . fmap show) rows

main :: IO ()
main = do
    input <- parseFile inputParser "app/Day14/Input.txt"
    rocks <- return $  fmap (\ p -> (p, Rock)) $ getAllRockPositions $ fmap makeRockFormation $ fromRight [] input
    let (total, drawing) = solve (0, initDrawing rocks)
    print total
    putStrLn $ render (488, 0) (512, 11) drawing

