module PointInShape exposing (..)

type alias Point = {x: Float, y: Float}
type Shape2D
  = Circle {center: Point, radius: Float}
  | Rectangle {topLeftCorner: Point, bottomRightCorner: Point}
  | Triangle {pointA: Point, pointB: Point, pointC: Point}

distance 

pointInShape : Point -> Shape2D -> Bool
pointInShape shape point =
  let
    (Point x y) = point
  in
    case shape of 
      Circle {center, radius} -> 

