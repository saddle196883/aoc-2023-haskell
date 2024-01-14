import Data.List (transpose)
import qualified Data.Map as M

main = do
    grid <- transpose . lines <$> getContents
    print $ getLoad $ shift grid
    let states = iterate shiftCycle grid
        (cycleStart, cycleLength) = findCycle states
        x = cycleStart + ((1000000000 - cycleStart) `rem` cycleLength)
    print $ getLoad $ states !! x

findCycle :: Ord a => [a] -> (Int, Int)
findCycle = helper M.empty 0
    where
        helper :: Ord a => M.Map a Int -> Int -> [a] -> (Int, Int)
        helper m n [] = (-1, -1)
        helper m n (x:xs) = case M.lookup x m of 
                              Just v -> (v, n - v)
                              Nothing -> helper (M.insert x n m) (n + 1) xs

shiftCycle :: [String] -> [String]
shiftCycle = rotate . shift . rotate . shift . rotate . shift . rotate . shift

getLoad :: [String] -> Int
getLoad xss = sum
    [sum $ zipWith (*) [1..] [if c == 'O' then 1 else 0 | c <- reverse xs]
      | xs <- xss]

shift :: [String] -> [String]
shift = map shiftOne

shiftOne :: String -> String
shiftOne = helper 0 0
    where
        helper n k "" = resolve n k
        helper n k ('#':xs) = resolve n k ++ "#" ++ helper 0 0 xs
        helper n k ('O':xs) = helper (n + 1) k xs
        helper n k ('.':xs) = helper n (k + 1) xs

        resolve n k = replicate n 'O' ++ replicate k '.'

rotate :: [[a]] -> [[a]]
rotate = reverse . transpose
