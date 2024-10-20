module Practice exposing (..)

safeDiv : Int -> Int -> Maybe Int
safeDiv a b = 
  if b == 0 then
    Nothing
  else
    Just (a // b)

len : List Int -> Int
len lx = 
  case lx of 
    [] -> 0
    x::xs -> 1 + len xs

lenTail : List Int -> Int
lenTail lx =
  let
    lenTailHelper ly a =
      case ly of 
        [] -> a
        x::xs -> lenTailHelper xs (a + 1)

  in
    lenTailHelper lx 0

last : List Int -> Maybe Int
last lx = 
  case lx of
    [] -> Nothing
    [x] -> Just x
    x::xs -> last xs

indexList : List Int -> Int -> Maybe Int
indexList lx i = 
  let
    helper ly j cnt = 
      case ly of
        [] -> Nothing
        x::xs -> 
          if j == cnt then
            Just x
          else 
            helper xs j (cnt + 1)    
  in 
    if i < 0 then
      Nothing
    else
      helper lx i 0
  