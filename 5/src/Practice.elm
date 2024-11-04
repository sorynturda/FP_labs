module Practice exposing (..)

import Lists exposing (map, foldl)

all : (a -> Bool) -> List a -> Bool
all pred l =
  l 
    |> map pred
    |> foldl (&&) True

any : (a -> Bool) -> List a -> Bool
any pred l =
  l 
    |> map pred
    |> foldl (||) False

type alias Point = {x: Int, y: Int, z: Int}
type Color = Red | Green | Blue

type alias ColoredSphere = {center: Point, color: Color, radius: Int}

moveUpdate : ColoredSphere -> Int -> Int -> ColoredSphere
moveUpdate sphere dx dy =
  let
    oldCenter = sphere.center
    newCenter = {oldCenter | x = oldCenter.x + dx, y = oldCenter.y + dy}
  in
    {sphere | center = newCenter}

chunks : Int -> List a -> List (List a)
chunks n xs =
    if xs == [] then
        []
    else
        let (head, tail) = splitAt n xs
        in head :: chunks n tail

splitAt : Int -> List a -> (List a, List a)
splitAt n l =
  case (n, l) of
    (0, xs) -> ([], xs)
    (_, []) -> ([], [])
    (m, (x :: xs)) ->
      let (ys, zs) = splitAt (m - 1) xs
      in (x :: ys, zs)