module Main where

import qualified Data.Vector as Vector
import Control.Monad ((>=>))
import Prelude hiding (Left, Right)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

readInt :: Char -> Int
readInt c = read [c]

type Grid a = Vector.Vector (Vector.Vector a)
type Position = (Int, Int)

get :: Position -> Grid a -> Maybe a
get (x, y) = (Vector.!? y) >=> (Vector.!? x)

height :: Grid a -> Int
height = Vector.length

width :: Grid a -> Int
width = Vector.length . Vector.head

data Direction = Up | Down | Left | Right deriving (Show, Eq)

next :: Direction -> Position -> Position
next Up (x, y) = (x, y - 1)
next Down (x, y) = (x, y + 1)
next Left (x, y) = (x - 1, y)
next Right (x, y) = (x + 1, y)

type TallestTree = Int

getVisible' ::  Grid Int -> Direction -> Int -> Position -> [Position]
getVisible' grid direction heightLimit position =
   case get position grid of
         Nothing -> []
         Just tree ->
              if tree < heightLimit
                then position : getVisible' grid direction heightLimit (next direction position)
                else position : []

getVisible :: Grid Int -> Direction -> Position -> [Position]
getVisible grid direction startPosition =
    case get startPosition grid of
        Nothing -> []
        Just height -> getVisible' grid direction height (next direction startPosition) 

scenicScore :: Grid Int -> Position -> Int
scenicScore grid position =
    let visibleUp = getVisible grid Up position
        visibleLeft = getVisible grid Left position
        visibleRight = getVisible grid Right position
        visibleDown = getVisible grid Down position
    in  product $ fmap length [visibleUp,  visibleLeft, visibleRight, visibleDown]

readInput ::IO (Grid Int)
readInput = do
    input <- readFile "app/Day08/Input.txt"
    return $ Vector.fromList $ fmap Vector.fromList $  (fmap . fmap) readInt $ lines input


solve :: Grid Int -> Int
solve grid =
    let allPositions = [(x, y) | x <- [0..width grid - 1], y <- [0..height grid - 1]]
        scores = fmap (scenicScore grid) allPositions
    in  maximum scores

main = do
    input <- readInput
    print $ solve input

