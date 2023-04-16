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


darkRed : Color
darkRed =
    toColor Color.darkRed


lightRed : Color
lightRed =
    toColor Color.lightRed


white : Color
white =
    toColor Color.white


green : Color
green =
    toColor Color.green


toColor : Color.Color -> Color
toColor color =
    color
        |> Color.toRgba
        |> El.fromRgb
