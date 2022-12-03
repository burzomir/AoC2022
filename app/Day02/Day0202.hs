{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import Text.Parsec ( char, (<|>), Parsec )
import Prelude hiding (round)
import Helpers ( parseInputFile )
import Data.Function ( (&) )

data Shape
    = Rock
    | Paper
    | Scissors
    deriving (Show)

data ExpectedResult
    = Lose
    | Draw
    | Win
    deriving (Show)

data Player
    = Opponent
    | Player

data Move p = Move Shape deriving (Show)

data Round = Round (Move Opponent) ExpectedResult (Move Player) deriving (Show)

choseMove :: Move Opponent -> ExpectedResult -> Move Player
choseMove (Move Rock) Lose = Move Scissors
choseMove (Move Rock) Draw = Move Rock
choseMove (Move Rock) Win = Move Paper
choseMove (Move Paper) Lose = Move Rock
choseMove (Move Paper) Draw = Move Paper
choseMove (Move Paper) Win = Move Scissors
choseMove (Move Scissors) Lose = Move Paper
choseMove (Move Scissors) Draw = Move Scissors
choseMove (Move Scissors) Win = Move Rock


opponentMoveParser :: Parsec String u (Move Opponent)
opponentMoveParser = (char 'A' >> return (Move Rock))
            <|> (char 'B' >> return (Move Paper))
            <|> (char 'C' >> return (Move Scissors))

expectedResultParser :: Parsec String u (ExpectedResult)
expectedResultParser = (char 'X' >> return Lose)
          <|> (char 'Y' >> return Draw)
          <|> (char 'Z' >> return Win)

roundParser :: Parsec String u Round
roundParser = do
    opponentMove <- opponentMoveParser
    char ' '
    expectedResult <- expectedResultParser
    return (Round opponentMove expectedResult (choseMove opponentMove expectedResult))

class Score a where
    score :: a -> Int

instance Score Shape where
    score Rock = 1
    score Paper = 2
    score Scissors = 3

instance Score ExpectedResult where
    score Lose = 0
    score Draw = 3
    score Win = 6
instance Score Round where
    score (Round _ (r) (Move p)) =  score r + score p

main :: IO ()
main = do
  input <- parseInputFile roundParser "app/Day02/Input.txt"
  input
    & (fmap . fmap ) score
    & fmap sum
    & print