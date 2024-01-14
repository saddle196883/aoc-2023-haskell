import Data.Either (fromRight)
import Data.List (sort)
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String

newtype Seed = Seed Integer deriving Show
data Range = Range Integer Integer deriving (Eq, Ord, Show)
data MapRange = MapRange Integer Integer Integer deriving (Eq, Ord, Show)
type Map = (M.Map MapRange ())
data Almanac = Almanac Map Map Map Map Map Map Map deriving Show

main = do
    stuff <- parseFromFile parseEverything "input.txt"
    case stuff of
        Left _ -> print "OH NO"
        Right (firstLine, almanac) -> do
            let eitherGarbageOrSeeds = parse parseSeeds "" firstLine
                seeds = fromRight [] eitherGarbageOrSeeds
                eitherGarbageOrRanges = parse parseRanges "" firstLine
                ranges = fromRight [] eitherGarbageOrRanges
            print $ getLowestLocationNumber almanac $ seedsToRanges seeds
            print $ getLowestLocationNumber almanac ranges

getLowestLocationNumber :: Almanac -> [Range] -> Integer
getLowestLocationNumber (Almanac a b c d e f g) ranges = getMinimum $ do
    range <- ranges
    soil <- useTable a range
    fertilizer <- useTable b soil
    water <- useTable c fertilizer
    light <- useTable d water
    temperature <- useTable e light
    humidity <- useTable f temperature
    useTable g humidity -- location
    where
        getMinimum = minimum . map (\(Range s _) -> s)

seedsToRanges :: [Seed] -> [Range]
seedsToRanges = map (\(Seed seed) -> Range seed (seed + 1))

useTable :: Map -> Range -> [Range]
useTable m (Range s e) = do
    MapRange ms me tr <- mapRanges
    return $ Range (tr + max ms s) (tr + min me e)
    where
        mapRanges = map fst $ M.toList $ M.filterWithKey
            (\(MapRange ms me tr) () -> (s < me) && (ms < e)) m

-- parser combinator hell
parseEverything :: Parser (String, Almanac)
parseEverything = do
    firstLine <- manyTill anyChar (try newline)
    many1 newline
    a : b : c : d : e : f : g : _ <- sepEndBy parseMapRanges (try newline)
    return (firstLine, Almanac a b c d e f g)

parseSeeds :: Parser [Seed]
parseSeeds = do
    manyTill anyChar (try space)
    fmap Seed <$> sepEndBy parseNumber (try space)

parseRanges :: Parser [Range]
parseRanges = do
    manyTill anyChar (try space)
    sepEndBy parseRange (try space)

parseRange :: Parser Range
parseRange = do
    start <- parseNumber
    space
    range <- parseNumber
    return $ Range start (start + range)

parseMapRanges :: Parser Map
parseMapRanges = do
    manyTill anyChar (try newline)
    stuff <- sepEndBy parseMapRange (try newline)
    return $ M.fromList $ zip (fill 0 $ sort stuff) (repeat ())
     where
         fill low [] = [MapRange low 100000000000 0]
         fill low (mr@(MapRange ms me tr) : xs)
           | ms == low = mr : fill (me + 1) xs
           | otherwise = MapRange low ms 0 : mr : fill (me + 1) xs

parseMapRange :: Parser MapRange
parseMapRange = do
    destStart <- parseNumber
    space
    sourceStart <- parseNumber
    space
    range <- parseNumber
    return $ MapRange
        sourceStart (sourceStart + range) (destStart - sourceStart)

parseNumber :: Parser Integer
parseNumber = read <$> many1 digit
