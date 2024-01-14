import qualified Data.Array as A
import qualified Data.Set as S

type Graph a = a -> [a]

main = do
    contents <- lines <$> getContents
    let
        grid = A.array ((1, 1), (length contents, length $ head contents)) $ do
            (x, line) <- zip [1..] contents
            (y, chr) <- zip [1..] line
            return ((x, y), chr)
        s = source grid
        bfsResult = lws (toGraph grid) s
        loop = map (!! 0) bfsResult
            ++ reverse (map (!! 1) $ init $ tail bfsResult)
            ++ head bfsResult
        area = abs (twiceSignedArea loop) `div` 2
        -- by picks theorem we can recover the number of interior points
        interiorPoints = (abs (twiceSignedArea loop) - length loop + 3) `div` 2
    print $ length bfsResult - 1
    print interiorPoints

toGraph :: A.Array (Int, Int) Char -> Graph (Int, Int)
toGraph array (x, y)
  | chr `elem` "JL7F-|" = [(x - 1, y) | chr `elem` "JL|", x > up]
    ++ [(x + 1, y) | chr `elem` "7F|", x < down]
    ++ [(x, y - 1) | chr `elem` "J7-", y > left]
    ++ [(x, y + 1) | chr `elem` "LF-", y < right]
  | chr == 'S' = [(x - 1, y) | x > up, array A.! (x - 1, y) `elem` "7F|"]
    ++ [(x + 1, y) | x < down, array A.! (x + 1, y) `elem` "JL|"]
    ++ [(x, y - 1) | y > left, array A.! (x, y - 1) `elem` "LF-"]
    ++ [(x, y + 1) | y < right, array A.! (x, y + 1) `elem` "J7-"]
  | otherwise = []
    where
        chr = array A.! (x, y)
        ((up, left), (down, right)) = A.bounds array

source :: A.Array (Int, Int) Char -> (Int, Int)
source array = fst $ head $ filter (\(_, chr) -> chr == 'S') $ A.assocs array

lws :: Ord a => Graph a -> a -> [[a]]
lws g r = f b r [] [] S.empty
  where
    f k x ls qs s
      | x `S.member` s = k ls qs s
      | otherwise = k (x : ls) (g x : qs) (S.insert x s)

    b _ [] _ = []
    b k qs s = k : foldl (foldl f) b qs [] [] s

-- Area is found by shoelace formula
twiceSignedArea :: [(Int, Int)] -> Int
twiceSignedArea ((x1, y1) : (x2, y2) : xs) =
    x1*y2 - x2*y1 + twiceSignedArea ((x2, y2) : xs)
twiceSignedArea _ = 0
