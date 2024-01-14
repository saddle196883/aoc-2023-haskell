import Data.Array
import Data.Graph
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector

main = do
    initialPositions <- map parseBlock . lines <$> getContents
    let graph = buildGraph initialPositions
        finalPositions = findFinalPositions initialPositions
        graph' = buildGraph' (length initialPositions - 1) finalPositions
        parents = transposeG graph'
    print $ length $ filter
        (all (\x -> (outdegree graph') ! x /= 1) . snd)
        (zip [0..] (elems parents))
    print $ sum $
        [length [n | n <- [0..1498], willFall n k graph'] | k <- [0..1498]]

type Point = (Int, Int, Int)
data Block = Block !Point !Point deriving (Show)

willFall :: Int -> Int -> Graph -> Bool
willFall n k graph
  | graph ! n == [] = False
  | graph ! n == [k] = True
  | otherwise = all (\x -> willFall x k graph) (graph ! n)

parseBlock :: String -> Block
parseBlock str = Block (a, b, c) (d, e, f)
    where
        [a, b, c, d, e, f]
            = map read $ concat $ map (splitOn ",") $ splitOn "~" str

buildGraph :: [Block] -> Graph
buildGraph bs = graph
    where
        maxB = length bs - 1
        isOver b1@(Block (_, _, z1) _) b2@(Block (_, _, z2) _) =
            if null (proj b1 `intersect` proj b2) then False else z1 > z2
            where
                proj (Block (x1, y1, _) (x2, y2, _))
                  = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
        dependencies
          = [(i, j) 
          | (i, bi) <- zip [0..] bs, (j, bj) <- zip [0..] bs, bi `isOver` bj]
        graph = buildG (0, maxB) dependencies
        
findFinalPositions :: [Block] -> Map Point Int
findFinalPositions bs = foldr insertBlock Map.empty pairs
    where
        vbs = Vector.fromList bs
        pairs = [(vbs Vector.! i, i) | i <- topSort $ buildGraph bs]
        insertBlock (b, n) m =
            let z = last [z
                    | z <- takeWhile
                        (\x -> all (not . (`Map.member` m))
                        (cubes (moveTo x b)))
                        (reverse [0..400])]
             in foldr (uncurry Map.insert) m [(c, n) | c <- cubes (moveTo z b)]

buildGraph' :: Int -> Map Point Int -> Graph
buildGraph' maxB m = buildG (0, maxB) dependencies
    where
        dependencies = nub [(i, j)
          | (p, i) <- Map.assocs m
          , Map.member (lower p) m
          , let j = m Map.! (lower p)
          , i /= j]

moveTo :: Int -> Block -> Block
moveTo z (Block (x1, y1, z1) (x2, y2, z2))
  = Block (x1, y1, z) (x2, y2, z2 - z1 + z)

lower :: Point -> Point
lower (x, y, z) = (x, y, z - 1)

cubes :: Block -> [Point]
cubes (Block (x1, y1, z1) (x2, y2, z2))
  = [(x, y, z) | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]
