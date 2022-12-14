
module Main where

import Text.Parsec hiding ((<|>))
import File
import Grid
import Data.Either
import Control.Applicative
import Data.Maybe

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

type Drawing = Grid Tile

initDrawing :: [RockFormation] -> Drawing
initDrawing rockFormations = 
    let maxX = (+ 1) $ maximum $ fmap (maximum . fmap fst) $ fmap getRockPositions rockFormations
        maxY = (+ 1) $ maximum $ fmap (maximum . fmap snd) $ fmap getRockPositions rockFormations
    in fromList $ replicate maxY $ replicate maxX Air

draw :: [(Position, Tile)] -> Drawing -> Drawing
draw tiles = update tiles

render :: Drawing -> String
render drawing = unlines $ fmap concat $ ((fmap . fmap) show) $ toList drawing

toRocks :: [RockFormation] -> [(Position, Tile)]
toRocks rockFormations = 
    let rockPositions = fmap getRockPositions rockFormations
        rockPositions' = fmap (\ (x, y) -> ((x, y), Rock)) $ concat rockPositions
    in rockPositions'

data SandGrainState
    = Falling
    | AtRest
    | InAbyss

dropSandGrain :: Position -> Drawing -> (Position, SandGrainState)
dropSandGrain (x, y) drawing = 
    let fall = 
            let fallInto p = if canFallInto p drawing
                    then Just $ dropSandGrain p drawing
                    else Nothing
                res = fallInto (x - 1, y + 1) <|> fallInto (x + 1, y + 1)
            in fromMaybe ((x, y), AtRest) res
    in case get (x, y + 1) drawing of
        Just SandSource -> ((x, y), AtRest)
        Just Air -> dropSandGrain (x, y + 1) drawing
        Nothing -> ((x, y), InAbyss)
        Just Rock -> fall
        Just SandGrain -> fall
            

canFallInto :: Position -> Drawing -> Bool
canFallInto (x, y) drawing = 
    let tile = get (x, y) drawing
    in case tile of
        Just Air -> True
        Just Rock -> False
        Just SandSource -> False
        Just SandGrain -> False
        Nothing -> False

step :: Drawing -> Drawing
step drawing =
    let (finalPosition, _) = dropSandGrain (500, 1) drawing
    in draw [(finalPosition, SandGrain)] drawing

simulate :: Int -> Drawing -> Drawing
simulate steps =
    foldl (.) id $ replicate steps step

solve :: (Int, Drawing) -> (Int, Drawing)
solve (totalSteps, drawing) =
    let (position, state) = dropSandGrain (500, 1) drawing
    in case state of
        InAbyss -> (totalSteps, drawing)
        _ -> solve (totalSteps + 1, draw [(position, SandGrain)] drawing)

main :: IO ()
main = do
    input <- parseFile inputParser "app/Day14/Input.txt"
    let rockFormations = fmap makeRockFormation $ fromRight [] input
    let rocks = toRocks rockFormations
    let drawing = initDrawing rockFormations
    let sandSource = ((500, 0), SandSource)
    let tiles = sandSource:rocks
    let drawing' = draw tiles drawing
    let (total, drawing'') = solve (0, drawing')
    print total
    putStrLn $ render $ dropColumns 493 $ drawing''

