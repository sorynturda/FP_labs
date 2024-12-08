module Rezolvari where
import Data.Map (Map)
import Data.List
import qualified Data.Map as Map

a = fact 100000

fact 0 = 1 
fact n = factAcc n 1
factAcc n acc = 
    if n == 0 then
        acc 
    else
        factAcc (n-1) (n * acc)

myMap :: Integer -> Map Integer [Integer]
myMap n = Map.fromList (map makePair [1..n])
    where 
        makePair x = (x, [x])

capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

infixl 0 |>
x |> f = f x


sudan :: Int -> Int -> Int -> Int
sudan 0 x y = x + y
sudan n x y
    | (n > 0 && y == 0) = x
    | otherwise = sudan (n - 1) (s) (y + s) where s = sudan n x (y-1)

infixl 0 !&
x !& y = if x == y && x == True then False else True

-- (!&) :: Bool -> Bool -> Bool
-- x !& y = not (x && y)

-- (!&) :: Bool -> Bool -> Bool
-- True !& True = False
-- _ !& _ = True

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

average :: [Int] -> Float
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

countVowels :: String -> Int
-- countVowels "" = 0
-- countVowels s = length $ filter (\x -> elem x $ map (:[]) "aeiouAEIOU") $ map (:[]) s
countVowels s = length $ filter isVowel s
    where
        isVowel c = elem c "aeiouAEIOU"

addBigs :: [Int] -> [Int] -> [Int]
addBigs xs ys = reverse $ addWithCarry 0 (reverse xs) (reverse ys)
  where
    addWithCarry carry [] [] 
      | carry == 0 = []
      | otherwise = [carry]
    addWithCarry carry [] (y:ys) = (carry + y) `mod` 10 : addWithCarry ((carry + y) `div` 10) [] ys
    addWithCarry carry (x:xs) [] = (carry + x) `mod` 10 : addWithCarry ((carry + x) `div` 10) xs []
    addWithCarry carry (x:xs) (y:ys) = (sum `mod` 10) : addWithCarry (sum `div` 10) xs ys
      where sum = x + y + carry

breakToLines :: Int -> String -> [String]
breakToLines n s = splits (n `div` length s) s
  where
    splits _ [] = []
    splits l s = ((head $ pairToList $ splitAt n s)) : splits l (head $ tail $ pairToList $ splitAt n s)
      where pairToList (x,y) = [x, y]


formatLines :: [String] -> String
formatLines xs = intercalate "\n" xs