module Practice exposing (..)

enumerate : List a -> List (Int, a)
enumerate l =
  let 
    helper lx i acc =
      case lx of 
        [] -> acc
        x::xs -> helper xs (i+1) (acc ++ [(i, x)])
  in
    helper l 0 []

type Day = Mon | Tue | Web | Thu | Fri | Sat | Sun


repeat : Int -> a -> List a
repeat n elem = 
  let
    helper x i acc = 
      if i == 0 then
        acc
      else
        helper x (i-1) (acc ++ [x])
  in
    helper elem n []


countVowels : String -> Int
countVowels s =
  let 
    helper a cnt =
      case a of 
        [] -> cnt
        x::xs -> 
          if x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u' ||
            x == 'A' || x == 'E' || x == 'I' || x == 'O' || x == 'U' then
            helper xs (cnt+1)
          else
            helper xs cnt
  in
    helper (String.toList (s)) 0

partition : Int -> List Int -> (List Int, List Int)
partition pivot list =
  let
    partitionHelper accLess accGreater l =
      case l of 
        [] -> (accLess, accGreater)
        x::xs ->
          if x < pivot then
            partitionHelper (accLess ++ [x]) accGreater xs
          else
            partitionHelper accLess (accGreater ++ [x]) xs
  in
    partitionHelper [] [] list



countriesWithCapital : List (String, String) -> (String -> Bool) -> List String
countriesWithCapital l pred = 
  case l of
    [] -> []
    (x,y)::ls ->
      if (pred y) then
        x::countriesWithCapital ls pred
      else
        countriesWithCapital ls pred

