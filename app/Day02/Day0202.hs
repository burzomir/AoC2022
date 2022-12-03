{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import Text.Parsec
import Prelude hiding (round)
import Helpers

data Shape
    = Rock
    | Paper
    | Scissors
    deriving (Show)

data Result
    = Lost
    | Draw
    | Won
    deriving (Show)

data Player
    = Opponent
    | Player

data Move p = Move Shape deriving (Show)

data Round = Round (Move Opponent) (Move Player) Result deriving (Show)

result :: Move Opponent -> Move Player -> Result
result (Move Rock) (Move Paper) = Won
result (Move Paper) (Move Scissors) = Won
result (Move Scissors) (Move Rock) = Won
result (Move Rock) (Move Rock) = Draw
result (Move Paper) (Move Paper) = Draw
result (Move Scissors) (Move Scissors) = Draw
result _ _ = Lost

playerMove :: Parsec String u (Move Player)
playerMove = (char 'X' >> return (Move Rock))
          <|> (char 'Y' >> return (Move Paper))
          <|> (char 'Z' >> return (Move Scissors))

opponentMove :: Parsec String u (Move Opponent)
opponentMove = (char 'A' >> return (Move Rock))
            <|> (char 'B' >> return (Move Paper))
            <|> (char 'C' >> return (Move Scissors))

round :: Parsec String u Round
round = do
    opponent <- opponentMove
    char ' '
    player <- playerMove
    return (Round opponent player (result opponent player))

class Score a where
    score :: a -> Int

instance Score Shape where
    score Rock = 1
    score Paper = 2
    score Scissors = 3

instance Score Result where
    score Lost = 0
    score Draw = 3
    score Won = 6

instance Score Round where
    score (Round _ (Move p) r) = score p + score r

main :: IO ()
main = do
  input <- parseInputFile round "app/Day02/Input.txt"
  print $ (fmap sum) $ (fmap . fmap) score input