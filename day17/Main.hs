import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

main :: IO ()
main = do
    gridList <- map (map (\x -> (read [x] :: Int))) . lines <$> getContents
    let grid = V.map V.fromList (V.fromList gridList)
        graph = makeGraph 1 3 grid
        graph' = makeGraph 4 10 grid
        (minX, minY) = (0, 0)
        (maxX, maxY) = (V.length grid - 1, V.length (V.head grid) - 1)
        sources = [(minX, minY, dir) | dir <- [U, L, D, R]]
        targets = [(maxX, maxY, dir) | dir <- [U, L, D, R]]
    -- print graph
    print $ shortestPath graph sources targets
    print $ shortestPath graph' sources targets

data Direction = U | L | D | R deriving (Eq, Ord, Show)
type Node = (Int, Int, Direction)
type Grid = V.Vector (V.Vector Int)
type Graph = M.Map Node [(Node, Int)]

makeGraph :: Int -> Int -> Grid -> Graph
makeGraph lo hi !grid = M.fromList (map (\u -> (u, getNeighbours u)) nodes)
    where
        (minX, minY) = (0, 0)
        (maxX, maxY) = (V.length grid - 1, V.length (V.head grid) - 1)
        nodes = [ (x, y, dir)
                | x <- [minX..maxX] , y <- [minY..maxY] , dir <- [U, L, D, R]]
        getNeighbours (x, y, dir)
          | dir == U || dir == D = helper L ++ helper R
          | dir == L || dir == R = helper U ++ helper D
            where
                helper U =
                    [ ((x', y, U),
                        sum [grid V.! x'' V.! y | x'' <- [x'..x - 1]])
                    | x' <- [x - hi..x - lo]
                    , minX <= x' && x' <= maxX]
                helper L =
                    [ ((x, y', L),
                        sum [grid V.! x V.! y'' | y'' <- [y'..y - 1]])
                    | y' <- [y - hi..y - lo]
                    , minY <= y' && y' <= maxY]
                helper D =
                    [ ((x', y, D),
                        sum [grid V.! x'' V.! y | x'' <- [x + 1..x']])
                    | x' <- [x + lo..x + hi]
                    , minX <= x' && x' <= maxX]
                helper R =
                    [ ((x, y', R),
                        sum [grid V.! x V.! y'' | y'' <- [y + 1..y']])
                    | y' <- [y + lo..y + hi]
                    , minY <= y' && y' <= maxY]

shortestPath :: Graph -> [Node] -> [Node] -> Int
shortestPath graph sources targets = minimum $ map (finalDistances M.!) targets
    where
        infinity = 9999999999
        initialQueue = Seq.fromList sources
        initialDistances = M.fromList $
            [ (u, if u `elem` sources then 0 else infinity)
            | u <- M.keys graph]
        finalDistances = spfa initialQueue initialDistances

        spfa Seq.Empty distances = distances
        spfa (u Seq.:<| rest) distances =
            spfa newQueue newDistances
            where
                neighbours = graph M.! u
                updates = filter (\(v, d) -> d < distances M.! v)
                    $ map (\(v, w) -> (v, distances M.! u + w)) neighbours
                newQueue = (Seq.><) rest
                    $ Seq.fromList
                    $ filter (\u -> null (u `Seq.elemIndicesL` rest))
                    $ map fst updates
                newDistances = foldr (uncurry M.insert) distances updates
