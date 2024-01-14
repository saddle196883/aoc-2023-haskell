import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token

-- data types
type Time = Double
type Distance = Double
data Race = Race Time Distance deriving Show

-- driver code
main :: IO ()
main = do
    races <- parseFromFile parseInput1 "input.txt"
    case races of
      Left _ -> print "Oh no"
      Right rs -> print $ product $ map getNumberOfWays rs

    race <- parseFromFile parseInput2 "input.txt"
    case race of
      Left _ -> print "Oh no"
      Right r -> print $ getNumberOfWays r

-- the calculation function
getNumberOfWays :: Race -> Int
getNumberOfWays (Race t d) =
    if discriminant < 0
       then 0
       else upper - lower + 1
    where
        discriminant = t * t - 4 * d
        -- by lazy evaluation, the following won't run unless discriminant >= 0
        sqrtDiscriminant = sqrt discriminant
        lower = ceiling $ (t - sqrtDiscriminant) / 2.0
        upper = floor $ (t + sqrtDiscriminant) / 2.0

-- parsing
parseInput1 :: Parser [Race]
parseInput1 = zipWith Race <$> line <*> line
    where
        line = map read <$> parseLine

parseInput2 :: Parser Race
parseInput2 = Race <$> line <*> line
    where
        line = read . concat <$> parseLine

parseLine :: Parser [String]
parseLine = manyTill anyChar space *> spaces *> sepEndBy (many1 digit) spaces
