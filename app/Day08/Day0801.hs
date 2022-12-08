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
next Up (x, y) = (x, y + 1)
next Down (x, y) = (x, y - 1)
next Left (x, y) = (x + 1, y)
next Right (x, y) = (x - 1, y)

type TallestTree = Int

getVisible' ::  TallestTree -> Grid Int -> Direction -> Position ->   [Position]
getVisible' tallestTree grid direction  position   =
    case get position grid of
        Nothing -> []
        Just tree ->
            if tree > tallestTree
                then position : getVisible' tree grid direction  (next direction position)
                else getVisible' tallestTree grid direction  (next direction position)


getVisible :: Grid Int -> Direction -> Position -> [Position]
getVisible = getVisible' (-1)

findAllVisible :: Grid Int -> Set.Set Position
findAllVisible grid =
    let visibleFromUp = fmap (getVisible grid Up ) $ [(i, 0) | i <- [0..width grid -1]]
        visibleFromDown = fmap (getVisible grid Down) $ [(i, height grid - 1) | i <- [0..width grid -1]]
        visibleFromLeft = fmap (getVisible grid Left) $ [(0, i) | i <- [0..height grid -1]]
        visibleFromRight = fmap (getVisible grid Right) $ [(width grid - 1, i) | i <- [0..height grid -1]]
    in  Set.fromList $ concat $ fmap concat $ [visibleFromUp, visibleFromDown, visibleFromLeft, visibleFromRight]

readInput ::IO (Grid Int)
readInput = do
    input <- readFile "app/Day08/Input.txt"
    return $ Vector.fromList $ fmap Vector.fromList $  (fmap . fmap) readInt $ lines input


main = do
    input <- readInput
    print $ Set.size $ findAllVisible input

