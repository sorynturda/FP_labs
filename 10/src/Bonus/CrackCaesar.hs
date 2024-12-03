module CrackCaesar where

import qualified Data.Char as Char
import System.Environment (getArgs)
import Data.List (isPrefixOf)

shift :: Int -> Char -> Char
shift d c
  | Char.isAlpha c = tr c
  | otherwise = c where
      tr c = (Char.chr . sh . Char.ord . Char.toLower) c
      sh c = (c - shA + d) `mod` 26 + shA
      shA = Char.ord 'a'

decrypt :: Int -> String -> String
decrypt p c = map (shift (26 - p)) c

substr :: String -> String -> Bool
substr = undefined 

-- >>> substr "question" "To be or not to be, that is the question"
-- True

-- >>> substr "that" "To be or not to be, that is the question"
-- True

main :: IO ()
main = error "TODO"
