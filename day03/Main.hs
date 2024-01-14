import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.Vector as V

type Grid = V.Vector (V.Vector Char)

main :: IO ()
main = do
    contents <- getContents
    let grid = V.fromList $ map V.fromList $ lines contents
    print $ numberPositions grid
    print $ sum $ findGearRatios grid

findPartNumbers :: Grid -> V.Vector Int
findPartNumbers xss = do
    pos <- numberPositions xss
    guard $ isPartNumber xss pos
    return $ posToNum xss pos

findGearRatios :: Grid -> [Int]
findGearRatios xss =
    map snd $ M.toList $
    M.map product $
    M.filter ((== 2) . length) $
        V.foldr (\(pos, val) acc -> M.insertWith (++) pos [val] acc) M.empty
        $ hits xss

hits :: Grid -> V.Vector ((Int, Int), Int)
hits xss = do
    nPos <- numberPositions xss
    sPos <- adjacentStars xss nPos
    return (sPos, posToNum xss nPos)

adjacentStars :: Grid -> (Int, Int, Int) -> V.Vector (Int, Int)
adjacentStars xss nPos = do
    sPos <- neighbourhood xss nPos
    guard (sPos `V.elem` starPositions xss)
    return sPos

isPartNumber :: Grid -> (Int, Int, Int) -> Bool
isPartNumber xss pos = any isSymbol $ do
    (i, j) <- neighbourhood xss pos
    return $ xss V.! i V.! j

neighbourhood :: Grid -> (Int, Int, Int) -> V.Vector (Int, Int)
neighbourhood xss (x, y, z) =
    let up = max 0 (x - 1)
        down = min (V.length xss - 1) (x + 1)
        left = max 0 (y - 1)
        right = min (length (V.head xss) - 1) (z + 1)
     in V.fromList [(i, j) | i <- [up..down], j <- [left..right]]

numberPositions :: Grid -> V.Vector (Int, Int, Int)
numberPositions xss = do
    x <- V.fromList [0..length xss - 1]
    y <- V.fromList [0..length (V.head xss) - 1]
    guard (isDigit (xss V.! x V.! y))
    guard (y == 0 || (not . isDigit) (xss V.! x V.! (y - 1)))
    let z = y + V.length (V.takeWhile isDigit $ V.drop y (xss V.! x)) - 1
    return (x, y, z) -- row x, columns y to z inclusive

starPositions :: Grid -> V.Vector (Int, Int)
starPositions xss = do
    x <- V.fromList [0..length xss - 1]
    y <- V.fromList [0..length (V.head xss) - 1]
    guard (xss V.! x V.! y == '*')
    return (x, y) -- row x, column y

posToNum :: Grid -> (Int, Int, Int) -> Int
posToNum xss (x, y, z) =
    read $ V.toList $ V.take (z - y + 1) $ V.drop y $ xss V.! x

isDigit :: Char -> Bool
isDigit = (`elem` "0123456789")

isSymbol :: Char -> Bool
isSymbol = not . (`elem` ".0123456789")
