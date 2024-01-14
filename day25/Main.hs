import Control.Monad
import Data.List (unfoldr)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
    graph <- parseGraph <$> getContents
    let minMinCuts = do
            s <- Map.keys graph
            t <- Map.keys graph
            guard (s /= t)
            let graph' = fordFulkerson s t graph
            guard (getFlow s graph' == 3)
            return $ minCut s t graph'
    case minMinCuts of
      [] -> print "oh no"
      (sSet, tSet):_ -> print $ (Set.size sSet) * (Set.size tSet)

type Graph = Map String (Map String (Int, Int))

parseGraph :: String -> Graph
parseGraph s = foldr (uncurry (Map.insertWith (Map.union))) Map.empty $ do
    row <- lines s
    let [node, neighbours] = splitOn ": " row
        neighbours' = splitOn " " neighbours
    n <- neighbours'
    kv <- [(node, Map.singleton n (1, 0)), (n, Map.singleton node (1, 0))]
    return kv

residual :: Graph -> Graph
residual = Map.map (Map.map (\(c, f) -> (c - f, 0)))

findPath :: String -> String -> Graph -> Maybe [String]
findPath s t g =
    if t `Map.member` dfsTree
       then Just $ reverse
        $ unfoldr (\v -> fmap (\x -> (x, x)) (Map.lookup v dfsTree)) t
       else Nothing
    where
        dfsTree = dfs Map.empty (Set.singleton s) [s]
        dfs parents seen [] = parents
        dfs parents seen (x:xs)
          | otherwise =
              let ns = [ n
                    | (n, (c, f)) <- Map.assocs (g Map.! x)
                    , c > f
                    , not (n `Set.member` seen)]
                  newParents
                    = Map.union parents (Map.fromList [(n, x) | n <- ns])
                  newSeen = Set.union seen (Set.fromList ns)
               in dfs newParents newSeen (ns ++ xs)

findBottleneck :: [String] -> Graph -> Int
findBottleneck [] graph = 0
findBottleneck (x:xs) graph = 
    let pairs = zip (x:xs) xs
        cfs = [cf | (n, n') <- pairs, let (cf, _) = graph Map.! n Map.! n']
     in case cfs of
          [] -> 0
          (cf:rest) -> foldr min cf rest

push :: [String] -> Int -> Graph -> Graph
push [] _ graph = graph
push (x:xs) df graph =
    let pairs = zip (x:xs) xs
        in foldr
            (\(n, n') -> Map.adjust (Map.adjust (\(c, f) -> (c, f + df)) n') n)
            graph
            pairs

fordFulkerson :: String -> String -> Graph -> Graph
fordFulkerson s t g =
    let gf = residual g
        path = findPath s t gf
     in case path of
          Nothing -> g
          Just p -> fordFulkerson s t (push p (findBottleneck p gf) g)

getFlow :: String -> Graph -> Int
getFlow s graph = sum $ [f | (_, f) <- Map.elems (graph Map.! s)]

minCut :: String -> String -> Graph -> (Set String, Set String)
minCut s t g =
    let gf = residual g
        search set [] = set
        search set (x : xs) = search (Set.insert x set)
            $ xs ++ [n
              | (n, (c, f)) <- Map.assocs (gf Map.! x)
              , not (n `Set.member` set)
              , c - f > 0]
        sSide = search Set.empty [s]
        tSide = (Map.keysSet g) Set.\\ sSide
     in (sSide, tSide)
