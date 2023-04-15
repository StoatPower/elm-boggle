module Palette exposing (..)

import Color
import Element as El exposing (Color)


gray : Color
gray =
    toColor Color.lightGray


darkGray : Color
darkGray =
    toColor Color.darkGray


red : Color
red =
    toColor Color.red


white : Color
white =
    toColor Color.white


toColor : Color.Color -> Color
toColor color =
    color
        |> Color.toRgba
        |> El.fromRgb
