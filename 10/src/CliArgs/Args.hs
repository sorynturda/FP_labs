module Args where
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Program arguments: " ++ show args
