import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Token

data Move = L | R deriving (Read, Show)
type Node = String
type Graph = M.Map Node (Node, Node)

main = do
    stuffOrGarbage <- parseFromFile parseFile "input.txt"
    case stuffOrGarbage of
      Left garbage -> print garbage
      Right (moves, graph) -> do
          let startingNodes = filter (\k -> k !! 2 == 'A') (M.keys graph)
          print $ getNumberOfMoves graph (cycle moves) ["AAA"]
          print $ getNumberOfMoves graph (cycle moves) startingNodes

-- for some reason the path after Z goes back to the starting node, but why?
getNumberOfMoves :: Graph -> [Move] -> [Node] -> Int
getNumberOfMoves graph moves nodes =
    let
        getZs node = map fst
            $ filter (\x -> snd x !! 2 == 'Z')
            $ zip [1..] (simulate graph node moves)
        paths = map getZs nodes
        heads = map head paths :: [Int]
     in foldr lcm 1 heads

simulate :: Graph -> Node -> [Move] -> [Node]
simulate graph node (move:ms) =
    let (l, r) = graph M.! node
     in case move of
          L -> l : simulate graph l ms
          R -> r : simulate graph r ms

parseFile :: Parser ([Move], Graph)
parseFile = (,) <$> moves <* newline <*> graph
    where
        moves = map (read . (:[])) <$> manyTill (oneOf "LR") (try newline)
        node = many upper :: Parser Node
        line = (\u v1 v2 -> (u, (v1, v2)))
            <$> node <* string " = ("
            <*> node <* string ", "
            <*> node <* string ")"
        graph = M.fromList <$> sepEndBy line (try newline)
