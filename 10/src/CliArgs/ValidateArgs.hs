module ValidateArgs where
import System.Environment (getArgs)
import Text.Read (readMaybe)


main :: IO ()
main = do
  nrStr:args <- getArgs
  let nr = readMaybe nrStr :: Maybe Int
  case nr of
    Nothing -> putStrLn "First argument must be a positive number!"
    Just n
      | n < 0 -> putStrLn "First argument must be a positive number!"
      | otherwise ->
        case compare (length args) n of
          LT -> putStrLn "Too few arguments!"
          EQ -> putStrLn "Just enough arguments, good job!"
          GT -> putStrLn "Too many arguments!"

