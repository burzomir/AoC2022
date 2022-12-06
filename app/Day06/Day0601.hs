module Main where

import Text.Parsec
import qualified Data.Set as Set
import Helpers

data Marker = Marker Char Char Char Char deriving (Show)

makeMarker :: Char -> Char -> Char -> Char -> Maybe Marker
makeMarker c1 c2 c3 c4 = 
    let uniqueChars = Set.fromList [c1, c2, c3, c4]
    in if Set.size uniqueChars == 4
        then Just $ Marker c1 c2 c3 c4
        else Nothing
  

markerParser :: Parsec String () Marker
markerParser = do
    c1 <- letter
    c2 <- letter
    c3 <- letter
    c4 <- letter
    case makeMarker c1 c2 c3 c4 of
        Just marker -> return marker
        Nothing -> fail "Invalid marker"

streamParser :: Parsec String () Int
streamParser = do
    manyTill letter (lookAhead $ try markerParser)
    markerParser
    position <- sourceColumn <$> getPosition
    return (position - 1)

main :: IO ()
main = do
    position <- parseFile streamParser "app/Day06/Input.txt"
    print position