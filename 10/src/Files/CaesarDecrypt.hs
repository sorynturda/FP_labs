module CaesarDecrypt where

import Caesar ( shift )

import System.Environment (getArgs)

decrypt :: Int -> String -> String
decrypt p c = map (shift (26 - p)) c

main :: IO ()
main = do
  [shAmountStr] <- getArgs
  let shAmount = read shAmountStr :: Int
  input <- getContents
  putStr $ decrypt shAmount input
