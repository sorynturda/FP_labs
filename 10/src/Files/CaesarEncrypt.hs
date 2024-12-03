module CaesarEncrypt where

import Caesar ( shift )

import System.Environment (getArgs)

encrypt :: Int -> String -> String
encrypt p c = map (shift p) c

main :: IO ()
main = do
  [shAmountStr] <- getArgs
  let shAmount = read shAmountStr :: Int
  input <- getContents
  putStr $ encrypt shAmount input
