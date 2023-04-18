module UI.Palette exposing (..)

import Color exposing (Color)
import Color.Manipulate as Color
import Element as El


type alias Alpha =
    Float



--
-- Nero, Atlantis, Chiffon, Shadow Green, Lisbon Brown
-- https://coolors.co/100b00-85cb33-efffc8-a5cbc3-3b341f
--


nero : El.Color
nero =
    El.rgb255 16 11 0


atlantis : El.Color
atlantis =
    El.rgb255 133 203 51


chiffon : El.Color
chiffon =
    El.rgb255 239 255 200


shadowGreen : El.Color
shadowGreen =
    El.rgb255 165 203 195


lisbonBrown : El.Color
lisbonBrown =
    El.rgb255 59 52 31


alizarinCrimson : El.Color
alizarinCrimson =
    El.rgb255 215 38 61


hoverColor : El.Color -> El.Color
hoverColor color =
    scaleColor { hslScale | lightnessScale = -0.2, saturationScale = 0.4 } color


scaleColor :
    { saturationScale : Float
    , lightnessScale : Float
    , alphaScale : Float
    }
    -> El.Color
    -> El.Color
scaleColor scales color =
    color
        |> El.toRgb
        |> Color.fromRgba
        |> Color.scaleHsl scales
        |> Color.toRgba
        |> El.fromRgb


hslScale :
    { saturationScale : Float
    , lightnessScale : Float
    , alphaScale : Float
    }
hslScale =
    { saturationScale = 0
    , lightnessScale = 0
    , alphaScale = 0
    }
