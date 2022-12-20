module Main where

import Text.Parsec
import File
import qualified Data.Map as Map
import Algorithm.Search (dijkstra)
import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import Data.List (sortOn, (\\))
import Data.Ord (Down(..))
import Debug.Trace
import qualified Data.Set as Set


-- VALVE

data Valve = Valve Name Rate Tunnels deriving (Show, Eq, Ord)

valveName :: Valve -> Name
valveName (Valve name _ _) = name

valveRate :: Valve -> Rate
valveRate (Valve _ rate _) = rate

valveTunnels :: Valve -> Tunnels
valveTunnels (Valve _ _ tunnels) = tunnels

valveOpeningCost = 1
tunnelMoveCost = 1

type Name = String
type Rate = Int
type Tunnels = [Name]

valveParser :: Parsec String () Valve
valveParser = do
  string "Valve "
  name <- many1 letter
  string " has flow rate="
  rate <- many1 digit
  (try $ string "; tunnel leads to valve ") <|> (try $ string "; tunnels lead to valves ")
  tunnels <- many1 letter `sepBy` string ", "
  return $ Valve name (read rate) tunnels

inputParser :: Parsec String () [Valve]
inputParser = valveParser `sepBy` endOfLine

-- VALVE MAP

type ValveMap = Map.Map Name Valve

insert :: Valve -> ValveMap -> ValveMap
insert valve@(Valve name _ _) map = Map.insert name valve map

lookup :: Name -> ValveMap -> Maybe Valve
lookup name map = Map.lookup name map

fromList :: [Valve] -> ValveMap
fromList = foldr insert Map.empty

getTunnels :: Name -> ValveMap -> Tunnels
getTunnels name map = 
    case Map.lookup name map of
        Just (Valve _ _ tunnels) -> tunnels
        Nothing -> []

getRate :: ValveMap -> Name -> Int
getRate valveMap valve =
    case lookup valve valveMap of
        Just (Valve _ rate _) -> rate
        Nothing -> 0

shortestPath :: Name -> Name -> ValveMap -> Maybe (Int, [Name])
shortestPath start end map =
    dijkstra (\ t -> getTunnels t map) (\ _ _ -> tunnelMoveCost) (== end) start

-- Travel costs

type TravelCostMap = Map.Map (Name, Name) Int

createTravelCostMap :: ValveMap -> TravelCostMap
createTravelCostMap map = 
    let valves = Map.keys map
        shortestPaths = [ (v1, v2, shortestPath v1 v2 map) | v1 <- valves, v2 <- valves , v1 /= v2 ]
        toTravelCost (v1, v2, Just (cost, _)) = ((v1, v2), cost)
        toTravelCost (v1, v2, Nothing) = ((v1, v2), 0)
    in Map.fromList $ fmap toTravelCost shortestPaths

removeBrokenValves :: ValveMap -> TravelCostMap -> TravelCostMap
removeBrokenValves valveMap travelCostMap =
    let workingTargetValve (_, v) _ =
            case lookup v valveMap of
                Just (Valve _ rate _) -> rate > 0
                Nothing -> False
    in Map.filterWithKey workingTargetValve travelCostMap


neighbourTravelCosts :: Name -> TravelCostMap -> [(Name, Int)]
neighbourTravelCosts start travelCostMap =
    fmap (\ ((_, to), cost) -> (to, cost)) $ Map.assocs $ Map.filterWithKey (\ (from, _) _ -> from == start) travelCostMap

travelCost :: Name -> Name -> TravelCostMap -> Maybe Int
travelCost from to costMap =
    Map.lookup (from, to) costMap

-- SOLUTION

data MyState = MyState 
    { valvesMap :: ValveMap
    , travelCostMap :: TravelCostMap
    , currentValve :: Name
    , timeLeft :: Int
    , openedValves :: Set.Set Name 
    , totalReleasedPressure :: Int
    } deriving (Eq, Ord)

instance Show MyState where
    show state = show (currentValve state, timeLeft state, openedValves state, totalReleasedPressure state)

myState :: ValveMap -> Name -> Int -> MyState
myState valvesMap currentValve timeLeft =
    let travelCostMap = removeBrokenValves valvesMap $ createTravelCostMap valvesMap
    in MyState valvesMap travelCostMap currentValve timeLeft Set.empty 0

allOpen :: MyState -> Bool
allOpen state =
    let workingValves = Set.fromList $ fmap snd $ Map.keys $ travelCostMap state
    in workingValves == (openedValves state)

timeout :: MyState -> Bool
timeout state = timeLeft state <= 0

unreleasedPressure :: MyState -> Int
unreleasedPressure state =
    let closedValves = filter (\ valve -> not (valveName valve `Set.member` openedValves state)) $ Map.elems (valvesMap state)
    in sum $ fmap valveRate closedValves

releasedPressure :: MyState -> Int
releasedPressure state =
    sum $ fmap (getRate (valvesMap state)) $ Set.toList (openedValves state)

timeToOpen :: Name -> MyState -> Int
timeToOpen valve state =
    let openingCost = if valve `elem` (openedValves state) then 0 else 1
        infinity = (maxBound :: Int)
        travelCost = fromMaybe infinity $ Map.lookup (currentValve state, valve) (travelCostMap state)
    in (openingCost + travelCost)

openValve :: MyState -> Name  -> MyState
openValve state nextValve =
    let newState = state { currentValve = nextValve
          , timeLeft = (timeLeft state - timeToOpen nextValve state) 
          , openedValves = Set.insert nextValve (openedValves state)
          , totalReleasedPressure = (totalReleasedPressure state) + ((releasedPressure state) * (timeToOpen nextValve state))
          }
    in trace (show newState) newState

nextValves :: MyState -> [Name]
nextValves state =
    fmap snd $ filter (\(from, _) -> from == currentValve state) $ Map.keys $ travelCostMap state

nextStates :: MyState -> [MyState]
nextStates state =
    fmap (openValve state) (nextValves state)

cost :: MyState -> MyState -> Int
cost from to =
    (timeToOpen (currentValve to) from) * (unreleasedPressure from)


main = do
    input <- parseFile inputParser "app/Day16/Input.txt"
    let valves =  fromRight [] input
    let valvesMap = fromList valves
    let state = myState valvesMap "AA" 30
    let optimalPath = dijkstra nextStates cost ((||) <$> allOpen <*> timeout) state
    let finalState = fmap ( head . reverse . snd) optimalPath
    let total = fmap ((\ state -> totalReleasedPressure state + (timeLeft state * releasedPressure state))) finalState
    print total
    
