module Palette exposing (..)

import Color
import Element as El exposing (Color)


type alias Alpha =
    Float



--
-- Nero, Atlantis, Chiffon, Shadow Green, Lisbon Brown
-- https://coolors.co/100b00-85cb33-efffc8-a5cbc3-3b341f
-- names from
--


nero : Color
nero =
    El.rgb255 16 11 0


atlantis : Color
atlantis =
    El.rgb255 133 203 51


chiffon : Color
chiffon =
    El.rgb255 239 255 200


shadowGreen : Color
shadowGreen =
    El.rgb255 165 203 195


lisbonBrown : Color
lisbonBrown =
    El.rgb255 59 52 31


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


lightBrown : Color
lightBrown =
    toColor Color.lightBrown


toColor : Color.Color -> Color
toColor color =
    color
        |> Color.toRgba
        |> El.fromRgb
