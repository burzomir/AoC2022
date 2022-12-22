{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Prelude hiding (Left, Right, round)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Control.Monad.Trans.State.Lazy as State
import Control.Monad (replicateM)

data Direction 
    = Down 
    | Left 
    | Right 
    deriving Show

class Move a where
    move :: Direction -> a -> a

data Position = Position {x :: Int, y :: Int} deriving (Eq)

instance Show Position where
    show (Position { x, y }) = show (x, y)

instance Move Position where
    move Down (Position {x, y}) = Position x (y - 1)
    move Left (Position {x, y}) = Position (x - 1) y
    move Right (Position {x, y}) = Position (x + 1) y

newtype Rock = Rock [Position] deriving (Eq)

instance Show Rock where
    show (Rock positions) = show positions

instance Move Rock where
    move direction (Rock positions) =  Rock $ fmap (move direction) positions

horizontalBar :: Position -> Rock
horizontalBar (Position { x, y }) =
    Rock $ [Position (x + 0) y, Position (x + 1) y, Position (x + 2) y, Position (x + 3) y]

cross :: Position -> Rock
cross (Position { x, y }) =
    Rock $ [Position (x + 1) (y + 2), Position x (y + 1), Position (x + 1) (y + 1), Position (x + 2) (y + 1), Position (x + 1) (y + 0)]

corner :: Position -> Rock
corner (Position { x, y }) =
    Rock $ [Position (x + 2) (y + 2), Position (x + 2) (y + 1), Position x (y + 0), Position (x + 1) (y + 0), Position (x + 2) (y + 0)]

verticalBar :: Position -> Rock
verticalBar (Position { x, y }) =
    Rock $ [Position x (y + 3), Position x (y + 2), Position x (y + 1), Position x (y + 0)]

square :: Position -> Rock
square (Position { x, y }) =
    Rock $ [Position x y, Position (x + 1) y, Position x (y + 1), Position (x + 1) (y + 1)]

initRockGenerator  :: [Position -> Rock]
initRockGenerator = List.cycle [horizontalBar, cross, corner, verticalBar, square]

nextRock :: Position -> [Position -> Rock] -> (Rock, [Position -> Rock])
nextRock position fallingRocks = ((List.head fallingRocks) position, List.drop 1 fallingRocks)

floorCollision :: Rock -> Bool
floorCollision (Rock positions) = List.any (\ Position { y } -> y < 1) positions

wallCollision :: Rock -> Bool
wallCollision (Rock positions) = List.any (\ Position { x } -> x < 1 || x > 7) positions

rockCollision :: Rock -> [Rock] -> Bool
rockCollision rock rocks =
        let collides (Rock positions1) (Rock positions2) = List.length (List.intersect positions1 positions2) > 0
        in Maybe.isJust $ List.find (collides rock) rocks

parseDirection :: Char -> Direction
parseDirection '<' = Left
parseDirection '>' = Right

initDirectionsGenerator :: String -> [Direction]
initDirectionsGenerator input = cycle $ fmap parseDirection input

nextDirection :: [Direction] -> (Direction, [Direction])
nextDirection directions = (List.head directions, List.drop 1 directions)

type Tower = [Rock]

towerHeight :: Tower -> Int
towerHeight [] = 0
towerHeight ((Rock positions):rocks) = maximum $ fmap y positions

data GameState = GameState
    { directionsGenerator :: [Direction]
    , rocksGenerator :: [Position -> Rock]
    , tower :: Tower
    , maxHeight :: Int
    }

initGameState :: String -> GameState
initGameState input =
    GameState (initDirectionsGenerator input) initRockGenerator [] 0

nextRockS :: State.State GameState Rock
nextRockS = do
    generator <- State.gets rocksGenerator
    height <- State.gets maxHeight
    let (rock, generator') = nextRock (Position 3 (height + 4)) generator
    State.modify (\ gameState -> gameState { rocksGenerator = generator' })
    return rock

nextDirectionS :: State.State GameState Direction
nextDirectionS = do
    generator <- State.gets directionsGenerator
    let (direction, generator') = nextDirection generator
    State.modify (\ gameState -> gameState { directionsGenerator = generator' })
    return direction

moveRockHorizontally :: Rock -> State.State GameState Rock
moveRockHorizontally rock = do
    direction <- nextDirectionS
    otherRocks <- State.gets tower
    let movedRock = move direction rock
    return $ if wallCollision movedRock || rockCollision movedRock otherRocks
        then rock
        else movedRock

moveRockVertically :: Rock -> State.State GameState Rock
moveRockVertically rock = do
    otherRocks <- State.gets tower
    let movedRock = move Down rock
    return $ if floorCollision movedRock || rockCollision movedRock otherRocks
        then rock
        else movedRock


moveRock :: Rock -> State.State GameState Rock
moveRock rock = do
    rock' <- moveRockHorizontally rock
    rock'' <- moveRockVertically rock'
    if rock' == rock''
        then return rock''
        else moveRock rock''


dropRock :: State.State GameState ()
dropRock = do
    rock <- nextRockS
    rock' <- moveRock rock
    mh <- State.gets maxHeight
    let rockMaxHeight (Rock positions) = maximum $ fmap y positions
    let newMaxHeight = if rockMaxHeight rock' > mh then rockMaxHeight rock' else mh
    State.modify (\ gameState -> gameState { tower = rock' : (tower gameState), maxHeight = newMaxHeight })

main = do
    input <- readFile "app/Day17/Input.txt"
    print $ maxHeight $ State.execState (replicateM 2022 dropRock) (initGameState input)
            