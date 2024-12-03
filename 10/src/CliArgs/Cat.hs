module Cat where

import System.Environment (getArgs)

data Args = Help | Files [String] deriving Show

parseArgs :: IO Args
parseArgs = do
  args <- getArgs
  return $ case args of
    [] -> Help
    args
      | "-h" `elem` args || "--help" `elem` args -> Help
      | otherwise -> Files args

helpMsg =
  "cat - concatenate files and print on the standard output\n" ++
  "Usage: cat [FILE]...\n" ++
  "Flags:\n" ++
  "   -h --help             Print this help message\n"


readFiles :: [String] -> IO [String]
readFiles files = sequenceA (map readFile files)



main :: IO ()
main = do
  args <- parseArgs
  case args of
    Help -> putStrLn helpMsg
    Files files -> do
      contents <- readFiles files
      putStrLn $ foldl (<>) mempty contents

