module ColoredSphere exposing (..)


type alias Point = {x: Int, y: Int, z: Int}
type Color = Red | Green | Blue

type alias ColoredSphere = {center: Point, color: Color, radius: Int}

moveUpdate : ColoredSphere -> Int -> Int -> ColoredSphere
moveUpdate sphere dx dy =
  let
    oldCenter = sphere.center
    newCenter = {oldCenter | x = oldCenter.x + dx, y= oldCenter.y + dy}
  in
    {sphere | center = newCenter}