import qualified Data.Matrix as Matrix -- external depedency, 'matrix' package
import Data.Ratio
import Data.List.Split (splitOn)

main :: IO ()
main = do
    snowballs <- map parseSnowball . lines <$> getContents
    let (lo, hi) = (200000000000000, 400000000000000)
    print $ length $ filter (uncurry (intersect2d lo hi))
        [ (s1, s2)
        | (i, s1) <- zip [1..] snowballs
        , (j, s2) <- zip [1..] snowballs
        , i < j]
    print $ findCorrectParameters snowballs

data Snowball = Snowball
    !Rational !Rational !Rational !Rational !Rational !Rational
    deriving (Eq, Show)

parseSnowball :: String -> Snowball
parseSnowball str = Snowball x y z dx dy dz
    where
        [[x, y, z], [dx, dy, dz]]
          = map (map (toRational . read) . splitOn ", ") $ splitOn " @ " str

intersect2d :: Rational -> Rational -> Snowball -> Snowball -> Bool
intersect2d lo hi (Snowball x1 y1 _ dx1 dy1 _) (Snowball x2 y2 _ dx2 dy2 _)
    -- non parallel
  | dx1 * dy2 /= dx2 * dy1 =
      let det = dx1 * dy2 - dx2 * dy1
          x = (dx1 * (dy2 * x2 - dx2 * y2) - dx2 * (dy1 * x1 - dx1 * y1)) / det
          y = (dy1 * (dy2 * x2 - dx2 * y2) - dy2 * (dy1 * x1 - dx1 * y1)) / det
          t1 = (x - x1) / dx1
          t2 = (x - x2) / dx2
       in t1 >= 0 && t2 >= 0 && lo <= x && x <= hi && lo <= y && y <= hi
    -- parallel, we assume lines do not coincide hehe xd rawr x3 nuzzles
  | otherwise = False

findCorrectParameters :: [Snowball] -> Maybe Rational
findCorrectParameters (s1 : s2 : s3 : s4 : _) =
    let
        (Snowball x1 y1 z1 dx1 dy1 dz1) = s1
        (Snowball x2 y2 z2 dx2 dy2 dz2) = s2
        (Snowball x3 y3 z3 dx3 dy3 dz3) = s3
        (Snowball x4 y4 z4 dx4 dy4 dz4) = s4
        a = Matrix.fromLists
            [ [-dy1, dx1, 0, y1, -x1, 0, 1, 0]
            , [-dz1, 0, dx1, z1, 0, -x1, 0, 1]
            , [-dy2, dx2, 0, y2, -x2, 0, 1, 0]
            , [-dz2, 0, dx2, z2, 0, -x2, 0, 1]
            , [-dy3, dx3, 0, y3, -x3, 0, 1, 0]
            , [-dz3, 0, dx3, z3, 0, -x3, 0, 1]
            , [-dy4, dx4, 0, y4, -x4, 0, 1, 0]
            , [-dz4, 0, dx4, z4, 0, -x4, 0, 1]]
        b = Matrix.fromList 8 1
            [ y1 * dx1 - x1 * dy1
            , z1 * dx1 - x1 * dz1
            , y2 * dx2 - x2 * dy2
            , z2 * dx2 - x2 * dz2
            , y3 * dx3 - x3 * dy3
            , z3 * dx3 - x3 * dz3
            , y4 * dx4 - x4 * dy4
            , z4 * dx4 - x4 * dz4
            ]
    -- I don't want to implement Gauss-Jordan, sorry
     in case Matrix.inverse a of
          Left _ -> Nothing
          Right m ->
              let (x : y : z : _) = Matrix.toList (m * b)
               in Just (x + y + z)
