module IO where

main = greet

greet = getLine >>= \name -> putStrLn ("Hello, " ++ name)

greetDo = do
  name <- getLine
  putStrLn $ "Hello, " ++ name

askNameAge = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn "How old are you?"
  ageStr <- getLine
  let 
    age = read ageStr :: Int

  if age < 20 then
    putStrLn ("'Sup, " ++ name)
  else
    putStrLn ("Hello, " ++ name)
