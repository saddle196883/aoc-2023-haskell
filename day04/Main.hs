import Data.List (intersect)
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String

main = do
    cards <- parseFromFile parseFile "input.txt"
    case cards of
      Left _ -> print "oh no"
      Right cs -> do
          print $ sum $ map getScore cs
          print $ getCards cs

data Card = Card Int [Int] [Int] deriving Show

getScore :: Card -> Int
getScore card = 
    let matches = getMatches card
     in if matches == 0 then 0 else 2 ^ (matches - 1)

getCards :: [Card] -> Int
getCards = helper M.empty
    where
        helper :: M.Map Int Int -> [Card] -> Int
        helper m [] = 0
        helper m (c@(Card n _ _) : cs) = copies + helper newM cs
            where
                matches = getMatches c
                copies = 1 + M.findWithDefault 0 n m
                newM = M.union m $
                    M.fromList $ [(n + k, copies) | k <- [1..matches]] 

getMatches :: Card -> Int
getMatches (Card _ xs ys) = length $ intersect xs ys

parseCard :: Parser Card
parseCard = do
    string "Card "
    many $ satisfy (== ' ')
    n <- many1 digit
    string ":"
    many1 $ satisfy (== ' ')
    xs <- sepEndBy (many1 digit) (many1 $ satisfy (== ' '))
    string "|"
    many1 $ satisfy (== ' ')
    ys <- sepEndBy (many1 digit) (many1 $ satisfy (== ' '))
    return $ Card (read n) (map read xs) (map read ys)

parseFile :: Parser [Card]
parseFile = sepEndBy parseCard newline
