module RevWords where
import System.Environment (getArgs)

revWords :: String -> String
revWords = unwords . map reverse . words

main :: IO ()
main = do
  [src] <- getArgs
  contents <- readFile src
  putStr (revWords contents)
  return ()
