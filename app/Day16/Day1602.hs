module Main where

import Text.Parsec
import File
import qualified Data.Map as Map
import Algorithm.Search (dijkstra)
import Prelude hiding (lookup, and, map)
import Data.Maybe (isJust)
import Data.Either (fromRight)
import Data.List (find, subsequences)
import Debug.Trace
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

-- VALVE

data Valve = Valve Name Rate Tunnels deriving (Show, Eq, Ord)

valveName :: Valve -> Name
valveName (Valve name _ _) = name

valveRate :: Valve -> Rate
valveRate (Valve _ rate _) = rate

valveTunnels :: Valve -> Tunnels
valveTunnels (Valve _ _ tunnels) = tunnels

tunnelMoveCost :: Int
tunnelMoveCost = 1

type Name = String
type Rate = Int
type Tunnels = [Name]

valveParser :: Parsec String () Valve
valveParser = do
  _ <- string "Valve "
  name <- many1 letter
  _ <- string " has flow rate="
  rate <- many1 digit
  _ <- (try $ string "; tunnel leads to valve ") <|> (try $ string "; tunnels lead to valves ")
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

trace' :: Show a => a -> a
trace' x = trace (show x) x

and :: (a -> Bool) -> (a -> Bool) -> a -> Bool
and f1 f2 = (&&) <$> f1 <*> f2

generatePaths :: ValveMap -> TravelCostMap -> Name -> [(Name, Int)] -> Int -> [[(Name, Int)]]
generatePaths valveMap costMap valve path timeLeft =
    let timeRemaining cost = timeLeft - cost - 1
        reachable (_, cost) =  timeRemaining cost >= 0
        onPath (valve', _) = isJust $ find (\ (valve'', _) -> valve' == valve'') path
        nextValves = filter (reachable `and` (not . onPath)) $ neighbourTravelCosts valve costMap
        nextPath = path ++  [(valve, timeLeft * (getRate valveMap valve))]
        handleNextValve (nextValve, cost) = generatePaths valveMap costMap nextValve nextPath (timeRemaining cost)
    in if length nextValves == 0
        then [nextPath]
        else concat $ fmap handleNextValve nextValves


disjointSets :: (Eq a, Ord a) => [a] -> [([a], [a])]
disjointSets fullSet =
    let set = Set.fromList fullSet
        subs = fmap Set.fromList $ subsequences fullSet
        match sub = 
            let matchingSub = find (\ s -> Set.union sub s == set) subs
            in (Set.toList sub, Set.toList $ fromMaybe Set.empty matchingSub )
    in fmap match subs


workingValves :: TravelCostMap -> [Name]
workingValves costMap = Set.toList $ Set.fromList $ fmap snd $ Map.keys costMap

disjointTravelCosts :: TravelCostMap -> ([Name], [Name]) -> (TravelCostMap, TravelCostMap)
disjointTravelCosts costMap (s1, s2) =
        ( Map.filterWithKey (\(k1a, k1b) _ -> (k1a `elem` s1 || k1a == "AA") && k1b `elem` s1) costMap
        , Map.filterWithKey (\(k2a, k2b) _ -> (k2a `elem` s2 || k2a == "AA") && k2b `elem` s2) costMap
        )

solve :: ValveMap -> TravelCostMap -> Int
solve valvesMap costMap = 
    maximum $ fmap sum $ (fmap . fmap) snd $ generatePaths valvesMap costMap "AA" [] 26

main :: IO ()
main = do
    input <- parseFile inputParser "app/Day16/Input.txt"
    let valves =  fromRight [] input
    let valvesMap = fromList valves
    let costMap = removeBrokenValves valvesMap $ createTravelCostMap valvesMap
    let disjointValves = disjointSets $ workingValves costMap
    let disjointCosts = fmap (disjointTravelCosts costMap) disjointValves
    let solution = maximum $ fmap (\ (c1, c2) -> (solve valvesMap c1 + solve valvesMap c2)) disjointCosts
    print solution
