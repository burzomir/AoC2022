module Main where

import qualified Data.Vector as Vector
import Control.Monad ((>=>))
import Algorithm.Search (dijkstra)

type HeightMap = Vector.Vector (Vector.Vector Elevation)
type Elevation = Char
type Position = (Int, Int)

get :: Position -> HeightMap -> Maybe Elevation
get (x, y) = (Vector.!? y) >=> (Vector.!? x)

height :: HeightMap -> Int
height = Vector.length

width :: HeightMap -> Int
width = Vector.length . Vector.head

neighbours :: Position -> HeightMap -> [Position]
neighbours (x, y) heightMap =
    let adjust elevation =
            case elevation of
                'S' -> 'a'
                'E' -> 'z'
                _ -> elevation

        getElevation position = 
            adjust <$> get position heightMap
        
        currentElevation = 
            getElevation (x, y) 

        candidates = 
            [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

        canStepOn position = 
            let nextElevation = getElevation position
                isTheSameOrLower = (nextElevation <= currentElevation)
                isOneStepHigher = (nextElevation == fmap succ currentElevation)
            in isTheSameOrLower || isOneStepHigher
    in 
        filter canStepOn candidates

solve input =
    let heightMap = Vector.fromList $ fmap Vector.fromList $ lines input
        getNeighbours position = neighbours position heightMap
        cost _ _ = 1
        isEnd position = get position heightMap == Just 'E'
        startPosition = (0, 20)
    in dijkstra getNeighbours cost isEnd startPosition

main = do
    input <- readFile "app/Day12/Input.txt"
    print $ solve input