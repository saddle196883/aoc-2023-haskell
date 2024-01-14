import Control.Monad (forM, replicateM)
import Data.List.Split (splitOn)
import Debug.Trace
import qualified Data.Array as A
import qualified Data.Vector as V

type Datum = (String, [Int])

main = do
    contents <- map parse . lines <$> getContents
    print $ sum $ map numberOfWays contents
    print $ sum $ map (numberOfWays . unfoldDatum) contents

parse :: String -> Datum
parse str =
    let [spring, numbers] = words str
        in (spring, map read $ splitOn "," numbers)

unfoldDatum :: Datum -> Datum
unfoldDatum (str, ns) = (
    str ++ "?" ++ str ++ "?" ++ str ++ "?" ++ str ++ "?" ++ str,
    ns ++ ns ++ ns ++ ns ++ ns)

numberOfWays :: Datum -> Int
numberOfWays (str, ns) = dp A.! (0, 0)
    where
        vstr = V.fromList (str ++ ".") -- add '.' for sanity reasons
        vns = V.fromList ns
        n = length vstr
        m = length ns
        dp = A.array ((0, 0), (n, m)) [((i, j), f i j) | i<-[0..n], j<-[0..m]]

        f i j
          | i == n && j == m = 1
          | i == n && j < m = 0
          | chr == '.' = dotCase
          | chr == '#' = hashCase
          | chr == '?' = dotCase + hashCase
            where
                chr = vstr V.! i
                count = vns V.! j
                dotCase = dp A.! (i + 1, j)
                hashCase = if j < m
                              && i + count <= n
                              && V.all (`elem` "?#") (V.slice i count vstr)
                              && vstr V.! (i + count) `elem` ".?"
                              then dp A.! (i + count + 1, j + 1) else 0

{-
numberOfWays :: Datum -> Int
numberOfWays (str, ns) = ways A.! (0, 0, 0)
    where
        vstr = V.fromList (str ++ ".") -- add '.' for sanity reasons
        vns = V.fromList ns
        n = length vstr
        m = length ns
        !ways = A.array ((0, 0, 0), (n, m, n)) [((i, j, k), helper i j k)
          | i <- [0..n], j <- [0..m], k <- [0..i]]
        helper i j k
        -- base cases
          | i == n && j == m && k == 0 = 1
          | i == n && j == m - 1 && k == V.last vns = 1
          | i >= n = 0 -- kill out of bounds cases
          | j >= m = 0 -- kill out of bounds cases
          | k > i = 0
        -- chr == '.'
          | chr == '.' && k == 0 = ways A.! (i+1, j, 0)
          | chr == '.' && k == count = ways A.! (i+1, j+1, 0)
          | chr == '.' = 0
        -- chr == '#'
          | chr == '#' = ways A.! (i+1, j, k+1)
        -- chr == '?'
          | chr == '?' && k == 0 =
              ways A.! (i+1, j, k+1) + ways A.! (i+1, j, 0)
          | chr == '?' && k == count =
              ways A.! (i+1, j, k+1) + ways A.! (i+1, j+1, 0)
          | chr == '?' = 0
          | otherwise = 0
            where
                chr = vstr V.! i
                count = vns V.! j
-}
