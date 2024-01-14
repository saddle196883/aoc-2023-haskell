import Data.List (partition, tails, unfoldr)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map

main = do
    contents <- splitOn [""] . lines <$> getContents
    let [workflowString, partString] = contents
        workflowMap = Map.fromList $ map parseWorkflow workflowString
        parts = map parseParts partString
        initRange = Range (1, 4000) (1, 4000) (1, 4000) (1, 4000)
        initString = "in"
    print $ sum $ map getRating $ solve workflowMap parts
    print $ sum $ map getSize $ solve workflowMap [(initRange, initString)]

data Coordinate = X | M | A | S
data Range = Empty | Range { xCoord :: (Int, Int)
                           , mCoord :: (Int, Int)
                           , aCoord :: (Int, Int)
                           , sCoord :: (Int, Int)} deriving (Eq, Show)
data Rule = Greater Coordinate Int | Smaller Coordinate Int
type Workflow = [(Range, String)]
type WorkflowMap = Map.Map String Workflow

-- range operations
validify :: Range -> Range
validify Empty = Empty
validify r@(Range rx rm ra rs) =
    if all (uncurry (<=)) [rx, rm, ra, rs]
       then r
       else Empty

intersectRange :: Range -> Range -> Range
intersectRange Empty _ = Empty
intersectRange _ Empty = Empty
intersectRange r1 r2 =
    let merge (x1, y1) (x2, y2) = (max x1 x2, min y1 y2)
        rx = xCoord r1 `merge` xCoord r2
        rm = mCoord r1 `merge` mCoord r2
        ra = aCoord r1 `merge` aCoord r2
        rs = sCoord r1 `merge` sCoord r2
     in validify (Range rx rm ra rs)

partitionRange :: Rule -> Range -> (Range, Range)
partitionRange (Greater X k) r =
    (validify $ r {xCoord = (bound, e)}, validify $ r {xCoord = (s, bound-1)})
    where
        (s, e) = xCoord r
        bound = max s (k + 1)
partitionRange (Greater M k) r =
    (validify $ r {mCoord = (bound, e)}, validify $ r {mCoord = (s, bound-1)})
    where
        (s, e) = mCoord r
        bound = max s (k + 1)
partitionRange (Greater A k) r =
    (validify $ r {aCoord = (bound, e)}, validify $ r {aCoord = (s, bound-1)})
    where
        (s, e) = aCoord r
        bound = max s (k + 1)
partitionRange (Greater S k) r =
    (validify $ r {sCoord = (bound, e)}, validify $ r {sCoord = (s, bound-1)})
    where
        (s, e) = sCoord r
        bound = max s (k + 1)
partitionRange (Smaller X k) r =
    (validify $ r {xCoord = (s, bound)}, validify $ r {xCoord = (bound+1, e)})
    where
        (s, e) = xCoord r
        bound = min e (k - 1)
partitionRange (Smaller M k) r =
    (validify $ r {mCoord = (s, bound)}, validify $ r {mCoord = (bound+1, e)})
    where
        (s, e) = mCoord r
        bound = min e (k - 1)
partitionRange (Smaller A k) r =
    (validify $ r {aCoord = (s, bound)}, validify $ r {aCoord = (bound+1, e)})
    where
        (s, e) = aCoord r
        bound = min e (k - 1)
partitionRange (Smaller S k) r =
    (validify $ r {sCoord = (s, bound)}, validify $ r {sCoord = (bound+1, e)})
    where
        (s, e) = sCoord r
        bound = min e (k - 1)

getRating :: Range -> Int
getRating Empty = 0
getRating (Range (sx, ex) (sm, em) (sa, ea) (ss, es)) = sum
    $ (\a b c d -> a + b + c + d)
    <$> [sx..ex] <*> [sm..em] <*> [sa..ea] <*> [ss..es]


getSize :: Range -> Int
getSize Empty = 0
getSize (Range (sx, ex) (sm, em) (sa, ea) (ss, es))
  = (ex - sx + 1) * (em - sm + 1) * (ea - sa + 1) * (es - ss + 1)

-- parsing
parseRule :: String -> Rule
parseRule (x : op : rest) =
    (if op == '>' then Greater else Smaller)
    (case x of 'x' -> X; 'm' -> M; 'a' -> A; 's' -> S)
    (read rest)

parseWorkflow :: String -> (String, Workflow)
parseWorkflow str =
    let [name, rest] = "{" `splitOn` init str
        conditions = "," `splitOn` rest
        final = last conditions
        ruleStrings = map (splitOn ":") (init conditions)
        rules = map (\[a, b] -> (parseRule a, b)) ruleStrings
        initialRange = Range (1, 4000) (1, 4000) (1, 4000) (1, 4000)
        helper range [] = [(range, final)]
        helper range ((rule, s):xs) =
            let (x, y) = partitionRange rule range
             in (x, s) : helper y xs
     in (name, helper initialRange rules)

parseParts :: String -> (Range, String)
parseParts xmas =
    let readMany = unfoldr $ listToMaybe . concatMap reads . tails
        [x, m, a, s] = readMany xmas :: [Int]
     in (Range (x, x) (m, m) (a, a) (s, s), "in")

-- driver code
project :: WorkflowMap -> String -> Range -> [(Range, String)]
project m s r = filter ((/= Empty) . fst)
    $ map (\(x,y) -> (x `intersectRange` r, y)) (m Map.! s)

solve :: WorkflowMap -> [(Range, String)] -> [Range]
solve _ [] = []
solve m ((r, s) : xs) =
    let (accepted, rest) = partition ((== "A") . snd) (project m s r)
        (rejected, rest2) = partition ((== "R") . snd) rest
     in map fst accepted ++ solve m (rest2 ++ xs)
