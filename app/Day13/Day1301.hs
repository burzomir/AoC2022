module Main where

import File
import Text.Parsec
import Data.Function

data PacketData 
    = PacketInt Int
    | PacketList [PacketData]

instance Show PacketData where
    show (PacketInt i) = show i
    show (PacketList l) = show l

instance Eq PacketData where
    (PacketInt i1) == (PacketInt i2) = i1 == i2
    (PacketList l1) == (PacketList l2) = l1 == l2
    (PacketList list) == (PacketInt int) = PacketList list == PacketList [PacketInt int]
    (PacketInt int) == (PacketList list) = PacketList [PacketInt int] == PacketList list

instance Ord PacketData where
    (PacketInt i1) <= (PacketInt i2) = i1 <= i2
    (PacketList l1) <= (PacketList l2) = l1 <= l2
    (PacketList list) <= (PacketInt int) = PacketList list <= PacketList [PacketInt int]
    (PacketInt int) <= (PacketList list) = PacketList [PacketInt int] <= PacketList list

type Parser a = Parsec String () a

packetIntParser :: Parser PacketData
packetIntParser = do
    i <- many1 digit
    return $ PacketInt (read i)

packetParser :: Parser PacketData
packetParser = do
    _ <- char '['
    p <- (packetIntParser <|> packetParser) `sepBy` char ','
    _ <- char ']'
    return $ PacketList p

packetPairParser :: Parser (PacketData, PacketData)
packetPairParser = do
    p1 <- packetParser
    _ <- endOfLine
    p2 <- packetParser
    return $ (p1, p2)

inputParser :: Parser [(PacketData, PacketData)]
inputParser = 
    packetPairParser `sepBy` (endOfLine >> endOfLine)

solve :: [(PacketData, PacketData)] -> Int
solve pairs =
    pairs
        & fmap (\ (p1, p2) -> p1 < p2)
        & zip [1..]
        & filter ((== True) . snd)
        & map fst
        & sum

main :: IO ()
main = do
    input <- parseFile inputParser "app/Day13/input.txt" 
    print $ solve <$> input
