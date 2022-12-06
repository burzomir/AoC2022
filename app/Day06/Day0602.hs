{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Text.Parsec
import qualified Data.Set as Set
import Helpers
import GHC.TypeLits
import Data.Proxy

data Marker l = Marker [Char] deriving (Show)

type PacketMarker = Marker 4
type MessageMarker = Marker 14

type Parser a = Parsec String () a

makeMarker :: forall (l :: Nat). KnownNat l => [Char] -> Maybe (Marker l)
makeMarker chars = 
    let uniqueChars = Set.fromList chars
        markerLength = fromIntegral $ natVal (Proxy @l)
    in if Set.size uniqueChars == markerLength
        then Just $ Marker chars
        else Nothing

parser :: forall (l :: Nat) . KnownNat l => Parser (Marker l)
parser = 
    let markerLength = fromIntegral $ natVal (Proxy @l)
    in do
        chars <- count markerLength letter
        case  makeMarker @l chars of
            Just marker -> return marker
            Nothing -> fail "Invalid marker"


streamParser :: Parsec String () Int
streamParser = do
    manyTill letter (lookAhead $ try (parser :: Parser MessageMarker))
    parser :: Parser MessageMarker
    messageMarkerPosition <- sourceColumn <$> getPosition
    return (messageMarkerPosition - 1)

main :: IO ()
main = do
    position <- parseFile streamParser "app/Day06/Input.txt"
    print position