module Grid where

import Data.Vector
import qualified Data.Vector as Vector
import Control.Monad
import Prelude hiding (length, head)

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