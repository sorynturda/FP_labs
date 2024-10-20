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
  
fibs : Int -> Int -> List Int
fibs start end = 
  let
    fibHelper x y f1 f2 i acc =
      if y < i then
        acc
      else
        if x < i then 
          fibHelper x y f2 (f1 + f2) (i + 1) (f1::acc)
        else
          fibHelper x y f2 (f1 + f2) (i + 1) acc
  in
    List.reverse (fibHelper start end 1 1 1 [])

fibs2 : Int -> Int -> List (Int, Int)
fibs2 start end = 
  let
    fibHelper x y f1 f2 i acc =
      if y < i then
        acc
      else
        if x < i then 
          fibHelper x y f2 (f1 + f2) (i + 1) ((i - 1, f1)::acc)
        else
          fibHelper x y f2 (f1 + f2) (i + 1) acc
  in
    List.reverse (fibHelper start end 1 1 1 [])





