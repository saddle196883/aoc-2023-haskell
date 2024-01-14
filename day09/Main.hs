type Pattern = [Integer]

main = do
    patterns <- map (map read . words) . lines <$> getContents :: IO [Pattern]
    print $ sum $ map predictForward patterns
    print $ sum $ map predictBackward patterns

predictForward :: Pattern -> Integer
predictForward xs = sum
    $ zipWith (\k p -> (n `choose` k) * p) [0..] (keyValues xs)
    where n = fromIntegral $ length xs

predictBackward :: Pattern -> Integer
predictBackward xs = sum
    $ zipWith (*) (cycle [1 , -1]) (keyValues xs)

keyValues :: Pattern -> [Integer]
keyValues [] = []
keyValues xs = head xs : keyValues (zipWith (-) (tail xs) xs)

choose :: Integer -> Integer -> Integer
n `choose` k =
    product [max (k+1) (n-k+1) .. n] `div` product [1 .. min k (n-k)]
