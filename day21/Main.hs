import qualified Data.Set as Set
import qualified Data.Vector as Vector

main = do
    contents <- getContents
    let graph = toGraph contents
    let steps = step (65, 65) graph
    -- Part 1 output
    print $ steps !! 64
    print (steps !! 65, steps !! (65 + 131), steps !! (65 + 2 * 131))
    -- then lagrange interpolate with an online calculator
    -- I cannot implement it, sorry

type Point = (Int, Int)
type Graph = Point -> [Point]

toGraph :: String -> Graph
toGraph xss = do
    let rows = fmap Vector.fromList (Vector.fromList (lines xss))
        fourDirections (i, j)
          = [(i + 1, j), (i - 1, j), (i, j - 1), (i, j + 1)]
        isSteppable (i', j')
          = (rows Vector.!? (i' `mod` 131)
            >>= (\x -> x Vector.!? (j' `mod` 131)))
            `elem` [Just '.', Just 'S']
        graph p = filter isSteppable (fourDirections p)
     in graph

step :: Point -> Graph -> [Int]
step p graph = map Set.size $ iterate (\xs ->
    (Set.unions $ map (\x -> Set.fromList (graph x)) (Set.toList xs)))
    (Set.singleton p)
