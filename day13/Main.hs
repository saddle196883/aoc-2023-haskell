import Data.List (transpose)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- splitOn [[]] . lines <$> getContents
    print $ sum $ map (head . findScores) contents
    print $ sum $ map (head . findNewScores) contents

findReflRows :: [String] -> [Int]
findReflRows xs = [i | i <- [1..length xs - 1]
                     , and $ zipWith (==) (reverse (take i xs)) (drop i xs)]

findScores :: [String] -> [Int]
findScores xs = ((100 *) <$> findReflRows xs) ++ findReflRows (transpose xs)

findNewScores :: [String] -> [Int]
findNewScores xs =
    let allPossibilities = generateAllPossibilities xs
        oldScore = head (findScores xs)
        allScores = concatMap findScores allPossibilities
        validScores = filter (\x -> x /= 0 && x /= oldScore) allScores
     in validScores

generateAllPossibilities :: [String] -> [[String]]
generateAllPossibilities xs = do
    i <- [0..length xs - 1]
    j <- [0..length (head xs) - 1]
    let
        oldRow = xs !! i
        newRow = take j oldRow
            ++ [if xs !! i !! j == '.' then '#' else '.']
            ++ drop (j + 1) oldRow
    return $ take i xs ++ [newRow] ++ drop (i + 1) xs
