module Main where

import Text.Parsec hiding (State)
import Data.Maybe (fromMaybe)
import Helpers

data Instruction
    = Noop
    | AddX Int
    deriving (Show)

data Device = Device
    { registerX :: Int
    , registerI :: Int
    } deriving (Show)

device :: Device
device = Device 
    { registerX = 1
    , registerI = 0
    }


clearRegisterI :: Device -> Device
clearRegisterI device = device 
        { registerX = registerX device + registerI device 
        , registerI = 0 
        }

setRegisterI :: Int -> Device -> Device
setRegisterI x device = device { registerI = x }

compileInstruction Noop = [Noop]
compileInstruction (AddX x) = [Noop, AddX x]

runInstruction :: Instruction -> Device -> Device
runInstruction Noop device = clearRegisterI device
runInstruction (AddX x) device = setRegisterI x $ clearRegisterI device

signalStrength :: (Int, Device) -> Int
signalStrength (cycle, device) = cycle * registerX device

solve :: [Instruction] -> Int
solve instructions =
    let instructions' = concat $ fmap compileInstruction $ instructions
        evaluationLog = drop 1 $ scanl (\ d i -> runInstruction i d) device instructions'
        cycles = zip [1..] evaluationLog
        every40thCycle = filter (\ (cycle, _) -> (cycle + 20) `rem` 40 == 0 ) cycles
    in  sum $ fmap signalStrength every40thCycle

main :: IO ()
main = do
    parsedInput <- parseFile input "app/Day10/Input.txt"
    print $ (\ instructions -> solve instructions)  <$> parsedInput

-- Parsing

type Parser a = Parsec String () a

noop :: Parser Instruction
noop = do
    string "noop"
    return Noop

addX :: Parser Instruction 
addX = do
    string "addx"
    spaces
    minus <- fromMaybe ' ' <$> (optionMaybe $ char '-')
    digits <- many1 digit
    return $ AddX $ read $ minus : digits

instruction :: Parser Instruction
instruction = addX <|> noop

input :: Parser [Instruction]
input = 
    instruction `sepBy` endOfLine




    

