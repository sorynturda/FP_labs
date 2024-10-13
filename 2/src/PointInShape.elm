module PointInShape exposing (..)
import Records exposing (heronShort)

type alias Point = {x: Float, y: Float}
type Shape2D
  = Circle {center: Point, radius: Float}
  | Rectangle {topLeftCorner: Point, bottomRightCorner: Point}
  | Triangle {pointA: Point, pointB: Point, pointC: Point}

heronShort a b c = 
  let 
    s = (a + b + c) / 2
  in 
    sqrt(s * (s - a) * (s - b) * (s - c))



pointInShape : Point -> Shape2D -> Bool
pointInShape point shape =
  let
    distance pointA pointB = 
      sqrt ((pointA.x - pointB.x)^2 + (pointA.y - pointB.y)^2)
  in
    case shape of 
      Circle {center, radius} ->  
        if distance {x=center.x, y=center.y} {x=point.x, y=point.y} < radius then
          True 
        else 
          False
      Rectangle {topLeftCorner, bottomRightCorner} -> 
        if (topLeftCorner.x < point.x && point.x < bottomRightCorner.x) &&
           (bottomRightCorner.y < point.y && point.y < topLeftCorner.y) then
          True
        else
          False
      Triangle {pointA, pointB, pointC} ->
        if ((heronShort (distance pointA pointB) (distance pointB pointC) (distance pointA pointC)) ==
            heronShort (distance pointA point) (distance point pointB) (distance pointA pointB) +
            heronShort (distance pointA point) (distance point pointC) (distance pointA pointC) +
            heronShort (distance pointB point) (distance point pointC) (distance pointB pointC)) then
          True
        else
          False

