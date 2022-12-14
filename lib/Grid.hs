{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Grid where

import Data.Vector hiding (foldr)
import qualified Data.Vector as Vector
import Control.Monad
import Prelude hiding (length, head)
import Data.List.Extra (groupSort)

type Grid a = Vector (Vector a)

type Position = (Int, Int)

get :: Position -> Grid a -> Maybe a
get (x, y) = (!? y) >=> (!? x)

height :: Grid a -> Int
height = length

width :: Grid a -> Int
width = length . head

fromList :: [[a]] -> Grid a
fromList = Vector.fromList . fmap Vector.fromList

toList :: Grid a -> [[a]]
toList = fmap Vector.toList . Vector.toList

dropColumns :: Int -> Grid a -> Grid a
dropColumns n = fmap (Vector.drop n)

update :: [(Position, a)] -> Grid a -> Grid a
update updates grid =
    let updates' = makeUpdates updates
        updateRow' (y, rowUpdates) = (y, updateRow rowUpdates $ (Vector.!) grid y)
        rows = fmap updateRow' updates'
    in (Vector.//) grid rows

updateRow :: [(Int, a)] -> Vector a -> Vector a
updateRow updates row = (Vector.//) row updates
    
makeUpdates :: [(Position, a)] -> [(Int, [(Int, a)])]
makeUpdates updates = 
    groupSort $ fmap (\ ((x, y), a) -> (y, (x, a))) updates