import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector

main = do
    grid <- Vector.fromList . map Vector.fromList . lines <$> getContents
    let graph = toGraph False grid
        graph' = toGraph True grid
        source = (0, 1)
        target = (Vector.length grid - 1, Vector.length (Vector.head grid) - 2)
    print $ longestPath graph source target
    print $ longestPath graph' source target

type Point = (Int, Int)
type Graph = Map Point [Point]
type WeightedGraph = Map Point [(Point, Int)]

toGraph :: Bool -> Vector (Vector Char) -> Graph
toGraph ignoreSlopes grid = Map.fromList $ do
    i <- [0..Vector.length grid - 1]
    j <- [0..Vector.length (Vector.head grid) - 1]
    let next =
            if grid !?!? (i, j) == Just '#'
               then []
               else if ignoreSlopes
               then [dir
                 | dir <- [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
                 , grid !?!? dir `elem` (Just <$> ".^>v<")]
                else
                [(i - 1, j) | grid !?!? (i - 1, j) `elem` (Just <$> ".^")]
                ++ [(i + 1, j) | grid !?!? (i + 1, j) `elem` (Just <$> ".v")]
                ++ [(i, j - 1) | grid !?!? (i, j - 1) `elem` (Just <$> ".<")]
                ++ [(i, j + 1) | grid !?!? (i, j + 1) `elem` (Just <$> ".>")]
    return ((i, j), next)
    where (!?!?) xss (i, j) = xss Vector.!? i >>= (\x -> x Vector.!? j)


getLandmarks :: Graph -> [Point]
getLandmarks = Map.keys . Map.filter (not . null . drop 2)

contract :: Graph -> Point -> Point -> WeightedGraph
contract graph source target = longestContractedGraph
    where
        landmarks = Set.fromList (source : target : getLandmarks graph)

        helper set node start
          | node `Set.member` landmarks && node /= start
            = [(node, Set.size set - 1)]
          | otherwise
            = concat
                [ helper (Set.insert next set) next start
                | next <- graph Map.! node
                , not (next `Set.member` set)]

        contractedGraph = Map.fromList
            $ [ (node, helper (Set.singleton node) node node)
              | node <- Set.toList landmarks]

        f = Map.toList . foldr (uncurry (Map.insertWith max)) Map.empty

        longestContractedGraph = Map.map f contractedGraph 

longestPath :: Graph -> Point -> Point -> Maybe Int
longestPath graph source target = helper (Set.empty) source 0
    where
        graph' = contract graph source target
        helper set node dist
          | node == target = Just dist
          | otherwise = (foldr max Nothing . map Just . catMaybes)
                [helper (Set.insert node set) next (dist + d)
                | (next, d) <- graph' Map.! node
                , not (next `Set.member` set)]
