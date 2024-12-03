module Caesar ( shift ) where

import qualified Data.Char as Char


shift :: Int -> Char -> Char
shift d c
  | Char.isAlpha c = tr c
  | otherwise = c where
      tr c = (Char.chr . sh . Char.ord . Char.toLower) c
      sh c = (c - shA + d) `mod` 26 + shA
      shA = Char.ord 'a'

