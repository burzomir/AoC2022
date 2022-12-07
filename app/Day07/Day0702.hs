module Main where

import qualified Data.Map as Map
import Data.Maybe
import Data.List (intercalate, filter, sortOn)
import Text.Parsec
import Helpers
import Data.Either (fromRight)

-- File System

type Name = String
type Size = Int

data FileSystem
    = Directory Name (Map.Map String FileSystem)
    | File Name Size

isDirectory :: FileSystem -> Bool  
isDirectory (Directory _ _) = True
isDirectory _ = False
    
instance Show FileSystem where
    show (Directory name _) = "Directory " ++ name
    show (File name _) = "File " ++ name

render' :: FileSystem -> [String]
render' (Directory name children) = 
        let directoryLine = " - " ++ name ++ " (dir)"
            childrenLines = (fmap . fmap) (\ l -> "  " ++ l) $ fmap render' $ Map.elems children
        in  directoryLine : concat childrenLines
render' (File name size) = [" - " ++ name ++ " (file, size=" ++ show size ++ ")"]

render :: FileSystem -> String
render fs = intercalate "\n" $ render' fs

getName :: FileSystem -> Name
getName (Directory name _) = name
getName (File name _) = name

data Context 
    = Top
    | Level Context FileSystem

type Location =  (Context, FileSystem)

root :: FileSystem
root = Directory "/" Map.empty

insert' :: FileSystem -> FileSystem -> FileSystem
insert' fs (Directory name children) =
        let newChildren = Map.insert (getName fs) fs children
        in Directory name newChildren

insert :: FileSystem -> Location -> Location
insert fs = fmap (insert' fs)

changeDirectory :: Name -> Location -> Location
changeDirectory ".." (Top, fs) = (Top, fs)

changeDirectory ".." (Level context parentFs, fs) = 
    (context, insert' fs parentFs)

changeDirectory nextDirName (context, currentDir@(Directory _ children)) = 
    fromMaybe (context, currentDir) $ do
        child <- Map.lookup nextDirName children
        case child of
            nextDir@(Directory _ _) -> 
                pure (Level context currentDir, nextDir)

            _ -> Nothing

top :: Location -> Location
top (Top, fs) = (Top, fs)
top location = top $ changeDirectory ".." location 

-- Commands

data Command
    = ChangeDirectory Name
    | List [FileSystem]

eval :: Command -> Location -> Location
eval (ChangeDirectory "/") location = top location
eval (ChangeDirectory name) location = changeDirectory name location
eval (List items) location = foldl (\ l fs -> insert fs l) location items


runScript :: [Command] -> Location
runScript commands = foldl (\ l c -> eval c l) (Top, root) commands

-- Input Parsing

type Parser a = Parsec String () a

nameParser :: Parser Name
nameParser = many (alphaNum <|> char '.' <|> char '/')

changeDirectoryParser :: Parser Command
changeDirectoryParser = do
    string "cd "
    name <- nameParser
    endOfLine
    return $ ChangeDirectory name

directoryParser :: Parser FileSystem
directoryParser = do
    string "dir "
    name <- nameParser
    endOfLine
    return $ Directory name Map.empty

fileParser :: Parser FileSystem
fileParser = do
    size <- read <$> many digit
    char ' '
    name <- nameParser
    endOfLine
    return $ File name size

listParser :: Parser Command
listParser = do
    string "ls"
    endOfLine
    items <- many (directoryParser <|> fileParser)
    return $ List items

commandParser :: Parser Command
commandParser = do
    string "$ "
    changeDirectoryParser <|> listParser

inputParser :: Parser [Command]
inputParser = do
    commands <- many commandParser
    eof
    return commands

-- Solution

totalDiskSpace = 70000000
updateSize = 30000000

getSize (_, x, _) = x
getTotal (_, _, x) = x
            
computeSize :: FileSystem -> (FileSystem, Size, [(FileSystem, Size)])
computeSize file@(File _ size) = (file, size, [])
computeSize directory@(Directory _ children) =
    let sizes = fmap computeSize $ Map.elems children
        total = sum $ fmap getSize sizes
        allTotals = concat $ fmap getTotal sizes
    in (directory, total, (directory, total) : allTotals)
        

solve :: FileSystem -> (FileSystem, Size)
solve fs =
    let (_, usedSpace, sizes) = computeSize fs
        freeSpace = totalDiskSpace - usedSpace
        missingSpace = updateSize - freeSpace
        toDelete = head $ sortOn snd $ filter (\ (_, s) -> s >= missingSpace) sizes
    in toDelete

main = do
    input <- parseFile inputParser "app/Day07/Input.txt"
    putStrLn $ fromRight "" $ fmap (show . solve . snd . top . runScript) input