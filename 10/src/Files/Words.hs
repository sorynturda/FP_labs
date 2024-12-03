module Words where
import System.Environment (getArgs)

countWords :: String -> Int
countWords = length . words

main :: IO ()
main = do
  [src] <- getArgs
  contents <- readFile src
  print (countWords contents)
  return ()
