import Text.Parsec (digit, eof, many1, sepBy, sepEndBy, string, try, (<|>))
import Text.Parsec.String (Parser, parseFromFile)

main = do
    games <- parseFromFile file "input.txt"
    let answer1 = case games of
                Left err -> -1
                Right gs -> sum $
                    map (\(Game g _) -> g) $ filter isPossible gs
    let answer2 = case games of
                Left err -> -1
                Right gs -> sum $ map power gs
    print answer1
    print answer2

isPossible :: Game -> Bool
isPossible (Game _ rs) = all helper rs
    where
        helper (Round r g b) = r <= 12 && g <= 13 && b <= 14

power :: Game -> Int
power (Game _ rs) =
    let
        (r, g, b) =
            foldr
            (\(Round i j k) (x, y, z) -> (max x i, max y j, max z k))
            (0, 0, 0)
            rs
     in r * g * b

data Color = Red Int | Green Int | Blue Int
data Round = Round Int Int Int
data Game = Game Int [Round]

buildRound :: [Color] -> Round
buildRound = helper (Round 0 0 0)
    where
        helper r [] = r
        helper (Round _ g b) ((Red r) : xs) = helper (Round r g b) xs
        helper (Round r _ b) ((Green g) : xs) = helper (Round r g b) xs
        helper (Round r g _) ((Blue b) : xs) = helper (Round r g b) xs

red :: Parser Color
red = do
    n <- many1 digit
    string " red"
    return $ Red (read n)

green :: Parser Color
green = do
    n <- many1 digit
    string " green"
    return $ Green (read n)


blue :: Parser Color
blue = do
    n <- many1 digit
    string " blue"
    return $ Blue (read n)

parseRound :: Parser Round
parseRound = do
    colors <- sepBy (try red <|> try green <|> try blue) (string ", ")
    return $ buildRound colors

game :: Parser Game
game = do
    string "Game "
    n <- many1 digit
    string ": "
    rounds <- sepBy parseRound (string "; ")
    return $ Game (read n) rounds

file :: Parser [Game]
file = do
    games <- sepEndBy game (try $ string "\n")
    eof
    return games
