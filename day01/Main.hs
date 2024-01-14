import Data.List (tails)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    contents <- getContents
    print $ sum $ map ((\ls -> 10 * head ls + last ls) . f1) (lines contents)
    print $ sum $ map ((\ls -> 10 * head ls + last ls) . f2) (lines contents)

f1 :: String -> [Int]
f1 = map (\x -> read [x]) . filter (`elem` "0123456789")

f2 :: String -> [Int]
f2 str =
    let
        helper :: String -> Maybe Int
        helper ('0':_) = Just 0
        helper ('1':_) = Just 1
        helper ('2':_) = Just 2
        helper ('3':_) = Just 3
        helper ('4':_) = Just 4
        helper ('5':_) = Just 5
        helper ('6':_) = Just 6
        helper ('7':_) = Just 7
        helper ('8':_) = Just 8
        helper ('9':_) = Just 9
        helper ('z':'e':'r':'o':_) = Just 0
        helper ('o':'n':'e':_) = Just 1
        helper ('t':'w':'o':_) = Just 2
        helper ('t':'h':'r':'e':'e':_) = Just 3
        helper ('f':'o':'u':'r':_) = Just 4
        helper ('f':'i':'v':'e':_) = Just 5
        helper ('s':'i':'x':_) = Just 6
        helper ('s':'e':'v':'e':'n':_) = Just 7
        helper ('e':'i':'g':'h':'t':_) = Just 8
        helper ('n':'i':'n':'e':_) = Just 9
        helper _ = Nothing
     in mapMaybe helper (tails str)
