import Data.List (elemIndex)
import Data.Maybe (fromJust)

main = do
    contents <- map
        ((\[x, y, z] -> (read x :: Direction, read y :: Int, init $ tail z))
        . words) . lines <$> getContents

    let format (dir, s, _) = (dir, s)
        path = turtle (map format contents)
    print $ countBoundary path + countInterior path

    let actualFormat (_, _, s) =
            (toEnum (read [last s]), hexToDec (init (tail s)))
        actualPath = turtle (map actualFormat contents)
    print $ countBoundary actualPath + countInterior actualPath

data Direction = R | D | L | U  deriving (Enum, Read)

turtle :: [(Direction, Int)] -> [(Int, Int)]
turtle = scanl helper (0, 0)
    where
        helper (x, y) (U, s) = (x - s, y)
        helper (x, y) (L, s) = (x, y - s)
        helper (x, y) (D, s) = (x + s, y)
        helper (x, y) (R, s) = (x, y + s)

hexToDec :: String -> Int
hexToDec = foldl (\acc x -> 16 * acc + fromJust (elemIndex x hexDigits)) 0
    where hexDigits = "0123456789abcdef"

countBoundary :: [(Int, Int)] -> Int
countBoundary = fst . foldr
    (\(x', y') (d, (x, y)) -> (d + abs (x - x') + abs (y - y'), (x', y')))
    (0, (0, 0))

countInterior :: [(Int, Int)] -> Int
countInterior xs = a + 1 - (b `div` 2)
    where
        b = countBoundary xs
        a = abs (getDoubleArea xs) `div` 2
        getDoubleArea [] = 0
        getDoubleArea [_] = 0
        getDoubleArea ((x1, y1) : (x2, y2) : xs) =
            x1 * y2 - y1 * x2 + getDoubleArea ((x2, y2) : xs)
