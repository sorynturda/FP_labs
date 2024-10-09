module Records exposing (..)
import List exposing (head)

type Dice = ONE | TWO | THREE | FOUR | FIVE | SIX

type alias DicePair = {diceOne: Dice, diceTwo: Dice}

type AnotherDicePair = AnotherDicePair Dice Dice

luckyRoll : DicePair -> String
luckyRoll {diceOne, diceTwo} = 
  case (diceOne, diceTwo) of
    (SIX, SIX) -> "Very lucky"
    (SIX, _) -> "Lucky"
    (_, SIX) -> "Lucky"
    (_, _) -> "Meh"

type ShapeRec 
  = CircleRec {radius: Float}
  | RectangleRec {width: Float, height: Float}
  | TriangleRec {sideA: Float, sideB: Float, sideC: Float}

heronShort a b c = 
  let 
    s = (a + b + c) / 2
  in 
    sqrt(s * (s - a) * (s - b) * (s - c))

areaRec : ShapeRec -> Float
areaRec shape = 
  case shape of
    CircleRec {radius} -> 2*pi*radius
    RectangleRec {width, height} -> width * height
    TriangleRec{sideA, sideB, sideC} -> heronShort sideA sideB sideC

