module Counter exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Html.Attributes exposing (disabled)

main =
  Browser.sandbox { init = 0, update = update, view = view }

type alias Model = Int

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1


view : Model -> Html Msg
view model =
  let
    bigFont = style "font-size" "20pt"
    fontColor = 
      if abs(model) > 8 then
        style "color" "red"
      else
        style "color" "black"
    upperLimit = model >= 10
    loweLimit = model <= -10
  in
    div []
      [ button [ bigFont,onClick Increment, disabled upperLimit] [ text "+" ]
      , div [ bigFont, fontColor] [ text (String.fromInt model) ]
      , button [ bigFont, onClick Decrement, disabled loweLimit ] [ text "-" ]
      ]
