module Main where 

import Text.Parsec hiding (State)
import Helpers
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.List (sortOn)
import Data.Ord
import Data.Function

main :: IO ()
main = do
    input <- parseFile inputParser "app/Day11/Input.txt"
    print $ solve <$> input

solve monkeys =
    monkeys
        & initState 
        & playRounds 10000
        & Map.elems
        & sortOn (Down . monkeyTotalItemsInspected)
        & take 2
        & (fmap monkeyTotalItemsInspected)
        & product

type State = Map.Map MonkeyId Monkey

leastCommonMultiple :: State -> Int
leastCommonMultiple state = 
    product $ fmap (divider . monkeyTest) $ Map.elems state
        

insertMonkey :: Monkey -> State -> State
insertMonkey monkey state = Map.insert (monkeyId monkey) monkey state

playRounds :: Int -> State -> State
playRounds n state = foldl (\state round -> round state) state $ replicate n playRound

playRound :: State -> State
playRound state =
    let monkeys = Map.keys state
        newState = foldl (\ state monkeyId -> playTurn' monkeyId state ) state monkeys
    in newState

playTurn' :: MonkeyId -> State -> State
playTurn' monkeyId state = 
    case Map.lookup monkeyId state of
        Nothing -> state
        Just monkey -> 
            let (newMonkey, throws) = playTurn monkey
                state1 = insertMonkey newMonkey state
                state2 = foldl (\ state throw -> catch' throw state ) state1 (manageWorryLevel state1 throws)
            in state2

manageWorryLevel :: State -> [Throw] -> [Throw]
manageWorryLevel state = fmap (\(monkeyId, item) -> (monkeyId, Item $ worryLevel item `mod` leastCommonMultiple state))

catch' :: Throw -> State -> State
catch' (monkeyId, item) state = 
    case Map.lookup monkeyId state of
        Nothing -> state
        Just monkey -> 
            let newMonkey = catch item monkey
            in Map.insert monkeyId newMonkey state

data Monkey = Monkey 
    { monkeyId :: MonkeyId
    , monkeyItems :: Seq.Seq Item
    , monkeyOperation :: Operation
    , monkeyTest :: Test
    , monkeyTotalItemsInspected :: Int
    } 

instance Show Monkey where
    show monkey = 
        "Monkey " ++ (show $ monkeyId monkey) ++ " inspected " ++ (show $ monkeyTotalItemsInspected monkey) ++ " items"

type MonkeyId = Int

type Throw = (MonkeyId, Item)

inspectItem :: Monkey -> Maybe (Monkey, Throw)
inspectItem monkey = 
    case Seq.lookup 0 $ monkeyItems monkey of
        Nothing -> Nothing
        Just item -> 
            let newWorryLevel = evalOperation (monkeyOperation monkey) (worryLevel item)
                monkeyToThrowTo = runTest (monkeyTest monkey) newWorryLevel
                newMonkey = monkey 
                    { monkeyItems = Seq.drop 1 $ monkeyItems monkey
                    }
                throw = (monkeyToThrowTo, Item newWorryLevel)
            in Just (newMonkey, throw)

playTurn ::  Monkey -> (Monkey, [Throw])
playTurn monkey = 
    case inspectItem monkey of
        Nothing -> (monkey, [])
        Just (newMonkey, throw) ->
            let (newMonkey2, throws) = playTurn newMonkey
                newMonkey3 = newMonkey2 
                    { monkeyTotalItemsInspected = monkeyTotalItemsInspected newMonkey2 + 1 
                    }
            in (newMonkey3, throw : throws)

catch :: Item -> Monkey -> Monkey
catch item monkey = 
    monkey { monkeyItems = monkeyItems monkey Seq.|> item }

data Item = Item 
    { worryLevel :: Int 
    } 

instance Show Item where
    show item = show $ worryLevel item

data Operation 
    = Val Int
    | Old 
    | Mul Operation Operation
    | Add Operation Operation
    deriving (Show)

evalOperation :: Operation -> Int -> Int
evalOperation (Val x) _= x
evalOperation Old old = old
evalOperation (Mul op1 op2) old = evalOperation op1 old * evalOperation op2 old
evalOperation (Add op1 op2) old = evalOperation op1 old + evalOperation op2 old


data Test = Test 
    { divider :: Int
    , ifTrue :: MonkeyId
    , ifFalse :: MonkeyId 
    } deriving (Show)

runTest :: Test -> Int -> MonkeyId
runTest test x = 
    if x `mod` divider test == 0 
        then ifTrue test 
        else ifFalse test

-- Parsing

type Parser a = Parsec String () a

monkeyIdParser :: Parser MonkeyId
monkeyIdParser = do
    string "Monkey "
    id <- read <$> many1 digit
    char ':'
    return id

monkeyItemsPaser :: Parser [Item]
monkeyItemsPaser = do
    string "  Starting items: "
    items <- many1 digit `sepBy` (string ", ")
    return $ fmap (Item . read) items

    
monkeyOperationParser :: Parser Operation
monkeyOperationParser = 
    let valParser :: Parser Operation
        valParser = (Val . read) <$> many1 digit

        oldParser :: Parser Operation
        oldParser = do
            string "old"
            return Old

        mulParser :: Parser Operation
        mulParser = do
            op1 <- (valParser <|> oldParser)
            string " * "
            op2 <- (valParser <|> oldParser)
            return $ Mul op1 op2
        
        addParser :: Parser Operation
        addParser = do
            op1 <- (valParser <|> oldParser)
            string " + "
            op2 <- (valParser <|> oldParser)
            return $ Add op1 op2

    in do
        string "  Operation: new = "
        op <- (try mulParser <|> try addParser)
        return op

monkeyTestParser :: Parser Test
monkeyTestParser = do
    string "  Test: divisible by "
    divider <- read <$> many1 digit
    endOfLine
    string "    If true: throw to monkey "
    ifTrue <- read <$> many1 digit
    endOfLine
    string "    If false: throw to monkey "
    ifFalse <- read <$> many1 digit
    return $ Test divider ifTrue ifFalse

monkeyParser :: Parser Monkey
monkeyParser = do
    id <- monkeyIdParser
    endOfLine
    items <- Seq.fromList <$> monkeyItemsPaser
    endOfLine
    operation <- monkeyOperationParser
    endOfLine
    test <- monkeyTestParser
    return $ Monkey id items operation test 0

inputParser :: Parser [Monkey]
inputParser = do
    monkeys <- monkeyParser `sepBy` (endOfLine >> endOfLine)
    eof
    return monkeys

initState :: [Monkey] -> State
initState monkeys = 
    foldl (\ state monkey -> insertMonkey monkey state) Map.empty monkeys
    