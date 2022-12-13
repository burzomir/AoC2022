module Main where

import File
import Text.Parsec
import Data.List

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
    compare (PacketInt i1) (PacketInt i2) = compare i1 i2
    compare (PacketList []) (PacketList []) = EQ
    compare (PacketList []) (PacketList (_:_)) = LT
    compare (PacketList []) (PacketInt _) = LT
    compare (PacketList (_:_)) (PacketList []) = GT
    compare (PacketInt _) (PacketList []) = GT
    compare (PacketList (p1:ps)) (PacketInt p2) = compare (PacketList (p1:ps)) (PacketList [PacketInt p2])
    compare (PacketInt p1) (PacketList (p2:ps)) = compare (PacketList [PacketInt p1]) (PacketList (p2:ps))
    compare (PacketList l1) (PacketList l2) = compare (head l1) (head l2) <> compare (tail l1) (tail l2)

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

inputParser :: Parser [PacketData]
inputParser = 
    packetParser `sepBy` many endOfLine

solve :: [PacketData] -> Int
solve packets =
    let divider n = PacketList $ [PacketList [PacketInt n]]
        packets' = packets ++ [divider 2, divider 6]
        sorted = zip ([1..] :: [Int]) $ sort packets'
        isDivider p = p == divider 2 || p == divider 6
    in product $ fmap (fst) $ filter (isDivider . snd) sorted

main :: IO ()
main = do
    input <- parseFile inputParser "app/Day13/input.txt" 
    print $ solve <$> input
