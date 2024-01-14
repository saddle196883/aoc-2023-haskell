import Control.Monad (guard)
import Data.List (transpose)

main :: IO ()
main = do
    grid <- lines <$> getContents
    print $ sumOfDistances grid 2
    print $ sumOfDistances grid 1000000

findExpandedLines :: [String] -> ([Int], [Int])
findExpandedLines grid = ( helper $ zip [1..] grid
                         , helper $ zip [1..] $ transpose grid
                         )
    where helper xs = [i | (i, x) <- xs, all (== '.') x]

findPoints :: [String] -> [(Int, Int)]
findPoints grid = do
    (x, row) <- zip [1..] grid
    (y, ele) <- zip [1..] row
    guard (ele == '#')
    return (x, y)

sumOfDistances :: [String] -> Int -> Int
sumOfDistances grid k = (`div` 2) $ sum $ do
    let points = findPoints grid
        (expandedRows, expandedColumns) = findExpandedLines grid
    (x1, y1) <- points
    (x2, y2) <- points
    let up = min x1 x2
        down = max x1 x2
        left = min y1 y2
        right = max y1 y2
        extraRows = length $
            filter (\x -> up < x && x < down) expandedRows
        extraColumns = length $
            filter (\y -> left < y && y < right) expandedColumns
    return $ down - up + right - left + ((k - 1) * (extraRows + extraColumns))
