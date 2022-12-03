module Helpers where

import Text.Parsec

parseFile :: Parsec String () a -> FilePath -> IO (Either ParseError a)
parseFile p fp = parse p fp <$> readFile fp

parseInputFile :: Parsec String () a -> FilePath -> IO (Either ParseError [a])
parseInputFile p = parseFile p'
  where p' = p `sepBy` endOfLine >>= \ps -> eof >> return ps