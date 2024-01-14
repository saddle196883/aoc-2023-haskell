import qualified Data.Array as A
import qualified Data.Set as S

main :: IO ()
main = do
    gridString <- lines <$> getContents
    let
        maxX = length gridString
        maxY = length (head gridString)
        grid = A.array
            ((1, 1), (length gridString, length (head gridString)))
            [((x, y), gridString !! (x - 1) !! (y - 1))
              | x <- [1..maxX] , y <- [1..maxY]]
        graph = makeGraph grid
        start = (1, 1, R)
        startingNodes =
            [(x, 1, R) | x <- [1..maxX]]
            ++ [(x, maxY, L) | x <- [1..maxX]]
            ++ [(1, y, D) | y <- [1..maxY]]
            ++ [(maxX, y, U) | y <- [1..maxY]]
    print $ hits graph start
    print $ maximum $ map (hits graph) startingNodes

data Direction = U | D | L | R deriving (Eq, Ord, Show)
type Node = (Int, Int, Direction)
type Graph a = a -> [a]

hits :: Graph Node -> Node -> Int
hits graph a = S.size $ S.fromList $ map (\(x, y, _) -> (x, y)) $ bfs graph a
    where
        bfs :: Ord a => Graph a -> Graph a
        bfs g ts = f ts b [] S.empty
          where
            f x fw bw s
              | S.member x s = fw bw s
              | otherwise      = x : fw (g x : bw) (S.insert x s)
            b [] _ = []
            b qs s = foldl (foldr f) b qs [] s

makeGraph :: A.Array (Int, Int) Char -> Node -> [Node]
makeGraph grid (x, y, dir) =
    case (grid A.! (x, y), dir) of
      ('.', U) -> filter isValid [(x - 1, y, U)]
      ('.', L) -> filter isValid [(x, y - 1, L)]
      ('.', D) -> filter isValid [(x + 1, y, D)]
      ('.', R) -> filter isValid [(x, y + 1, R)]
      ('|', U) -> filter isValid [(x - 1, y, U)]
      ('|', L) -> filter isValid [(x - 1, y, U), (x + 1, y, D)]
      ('|', D) -> filter isValid [(x + 1, y, D)]
      ('|', R) -> filter isValid [(x - 1, y, U), (x + 1, y, D)]
      ('-', U) -> filter isValid [(x, y - 1, L), (x, y + 1, R)]
      ('-', L) -> filter isValid [(x, y - 1, L)]
      ('-', D) -> filter isValid [(x, y - 1, L), (x, y + 1, R)]
      ('-', R) -> filter isValid [(x, y + 1, R)]
      ('\\', U) -> filter isValid [(x, y - 1, L)]
      ('\\', L) -> filter isValid [(x - 1, y, U)]
      ('\\', D) -> filter isValid [(x , y + 1, R)]
      ('\\', R) -> filter isValid [(x + 1, y, D)]
      ('/', U) -> filter isValid [(x, y + 1, R)]
      ('/', L) -> filter isValid [(x + 1, y, D)]
      ('/', D) -> filter isValid [(x, y - 1, L)]
      ('/', R) -> filter isValid [(x - 1, y, U)]
    where
        (_, (maxX, maxY)) = A.bounds grid
        isValid (x, y, _) = (x >= 1) && (y >= 1) && (x <= maxX) && (y <= maxY)
