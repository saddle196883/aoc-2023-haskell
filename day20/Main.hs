import Data.List (findIndices, partition, unfoldr)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

-- unfortunate code that relies on the fact that rx is fed by only
-- the conjunction xn, which is only fed by hn, mp, xf, fz...

main = do
    contents <- lines <$> getContents
    let state = buildNetwork contents
        nodes = network state
        outputs = unfoldr (Just . pushButton) state

    let allPulses = concat $ take 1000 outputs
        (lowPulses, highPulses) = partition (\(b, _, _) -> b) allPulses
    print $ length lowPulses * length highPulses

    let locate x = 1 + head (findIndices
            (any (\(b, from, to) -> b && from == x && to == "xn")) outputs)
        hn = locate "hn"
        mp = locate "mp"
        xf = locate "xf"
        fz = locate "fz"
    print $ hn `lcm` mp `lcm` xf `lcm` fz 

data Module = Broadcaster {outs :: [String]}
            | Flipflop {isOn :: Bool, outs :: [String]}
            | Conjunction {memory :: Map.Map String Bool, outs :: [String]}
            deriving (Show)
type Pulse = (Bool, String, String)
data NetworkState = NetworkState
    { network :: Map.Map String Module
    , pendingPulses :: Seq.Seq Pulse
    } deriving (Show)

parseModule :: String -> (String, Module)
parseModule (x : rest)
  | x == 'b' =
      ("broadcaster", Broadcaster $ splitOn ", " (splitOn " -> " rest !! 1))
  | x == '%' =
      let [name, targets] = splitOn " -> " rest
       in (name, Flipflop False $ splitOn ", " targets)
  | x == '&' =
      let [name, targets] = splitOn " -> " rest
       in (name, Conjunction Map.empty $ splitOn ", " targets)

buildNetwork :: [String] -> NetworkState
buildNetwork xs =
    let nodes = map parseModule xs
        unconnected = Map.fromList nodes
        adjustments = do
            (name, node) <- nodes
            out <- outs node
            let f node' = case node' of
                           Conjunction m xs ->
                                node' {memory = Map.insert name False m}
                           anything -> anything
            return (f, out)
        connected = foldr (uncurry Map.adjust) unconnected adjustments
     in NetworkState connected Seq.empty

pulse :: NetworkState -> Maybe (Pulse, NetworkState)
pulse (NetworkState _ Seq.Empty) = Nothing
pulse (NetworkState state (p@(isHigh, from, to) Seq.:<| queue)) =
    Just (p, NetworkState newState (queue Seq.>< Seq.fromList extras))
    where
        node = Map.lookup to state

        lowPulses = zip3 (repeat False) (repeat to)
        highPulses = zip3 (repeat True) (repeat to)

        newState = case newNode of
                     Just n -> Map.insert to n state
                     Nothing -> state

        newNode =
            case node of
              Just (Broadcaster _) -> node
              Just n@(Flipflop b _) ->
                  Just (if isHigh then n else n {isOn = not b})
              Just n@(Conjunction m _) ->
                  Just (n {memory = Map.insert from isHigh m})
              Nothing -> Nothing

        extras =
            case newNode of
              Just (Broadcaster xs) -> lowPulses xs
              Just (Flipflop True xs) -> if isHigh then [] else highPulses xs
              Just (Flipflop False xs) -> if isHigh then [] else lowPulses xs
              Just (Conjunction m xs) ->
                  if and (Map.elems m) then lowPulses xs else highPulses xs
              Nothing -> []

pushButton :: NetworkState -> ([Pulse], NetworkState)
pushButton state = (output, finalState)
    where
        initialQueue = Seq.singleton (False, "button", "broadcaster")
        result = unfoldr
            (fmap (\(a, b) -> ((a, b), b)) . pulse)
            (state {pendingPulses = initialQueue})
        output = map fst result
        finalState = snd $ last result
