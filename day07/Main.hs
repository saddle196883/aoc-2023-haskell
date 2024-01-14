import Data.Function ((&))
import Data.List (sortBy)
import Data.Ord (comparing)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Token

main = do
    handsOrGarbage <- parseFromFile parseFile "input.txt"
    case handsOrGarbage of
      Left garbage -> print garbage -- should not happen
      Right hands -> do
          hands
            & schwarztianSort (\hand@(Hand cards _) -> (getType hand, cards))
            & getWinnings
            & print
          hands
            & map jackToJoker
            & schwarztianSort (\hand@(Hand cards _) -> (getType hand, cards))
            & getWinnings
            & print

type Bid = Integer

data Card = Joker
          | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
          deriving (Eq, Ord, Enum, Show)

data Hand = Hand (Card, Card, Card, Card, Card) Bid deriving (Eq, Show)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind
              | FullHouse | FourOfAKind | FiveOfAKind
              deriving (Eq, Ord, Show)

getWinnings :: [Hand] -> Integer
getWinnings = sum . zipWith (\rank (Hand _ bid) -> rank * bid) [1..]

getType :: Hand -> HandType
getType hand = maximum $ map helper possibilities
    where
        possibilities = getAllPossibilities hand
        helper (Hand (a, b, c, d, e) _)
          | freqsOfFreqs !! 5 == 1 = FiveOfAKind
          | freqsOfFreqs !! 4 == 1 = FourOfAKind
          | freqsOfFreqs !! 3 == 1 && freqsOfFreqs !! 2 == 1 = FullHouse
          | freqsOfFreqs !! 3 == 1 = ThreeOfAKind
          | freqsOfFreqs !! 2 == 2 = TwoPair
          | freqsOfFreqs !! 2 == 1 = OnePair
          | otherwise = HighCard
            where
                getFreqs list = map (\x -> length (filter (== x) list))
                freqs = getFreqs [a, b, c, d, e] [Two ..]
                freqsOfFreqs = getFreqs freqs [0..5]

jackToJoker :: Hand -> Hand
jackToJoker (Hand (a, b, c, d, e) bid) =
    let
        [a', b', c', d', e'] =
            map (\x -> if x == Jack then Joker else x) [a, b, c, d, e]
     in Hand (a', b', c', d', e') bid

getAllPossibilities :: Hand -> [Hand]
getAllPossibilities (Hand (a, b, c, d, e) bid) = do
        a' <- if a == Joker then notJ else [a]
        b' <- if b == Joker then notJ else [b]
        c' <- if c == Joker then notJ else [c]
        d' <- if d == Joker then notJ else [d]
        e' <- if e == Joker then notJ else [e]
        return $ Hand (a', b', c', d', e') bid
    where
        notJ = [Two .. Ten] ++ [Queen, King, Ace]

schwarztianSort :: Ord a => (b -> a) -> [b] -> [b]
schwarztianSort f = map snd . sortBy (comparing fst) . map (\x -> (f x, x))

parseFile :: Parser [Hand]
parseFile = sepEndBy hand (try newline)
    where
        card = do
            c <- oneOf "23456789TJQKA"
            return $ case c of
                       'T' -> Ten
                       'J' -> Jack
                       'Q' -> Queen
                       'K' -> King
                       'A' -> Ace
                       n -> toEnum (read [n] - 1)
        cards = (,,,,) <$> card <*> card <*> card <*> card <*> card
        bid = read <$> many1 digit
        hand = Hand <$> cards <* space <*> bid
