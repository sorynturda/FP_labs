module LowerCase where

import System.Environment (getArgs)
import Data.Char (toLower)


main :: IO ()
main = do
  [src, dst] <- getArgs
  putStrLn $ "Copying from " ++ src ++ " to " ++ dst
  contents <- readFile src
  writeFile dst $ map toLower contents
  putStrLn "Done"

