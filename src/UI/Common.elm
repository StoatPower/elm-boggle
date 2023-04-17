module UI.Common exposing (..)

import Element exposing (Attr, Element, centerX, centerY, el, height, px, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Message exposing (Msg(..))
import Scorebook exposing (Score)
import UI.Palette exposing (..)


standardButtonView :
    { opts
        | attrs : List (Attr () Msg)
        , onPress : Maybe Msg
        , text : String
    }
    -> Element Msg
standardButtonView opts =
    Input.button
        ([ width <| px 250
         , height <| px 55
         , Border.rounded 5
         , Font.letterSpacing 2
         ]
            ++ opts.attrs
        )
        { onPress = opts.onPress
        , label = el [ centerX, centerY ] <| text opts.text
        }


scoreColor : Score -> Attr () Msg
scoreColor score =
    if score < 0 then
        Font.color alizarinCrimson

    else if score == 0 then
        Font.color chiffon

    else
        Font.color atlantis


padNumber : Int -> String
padNumber time =
    if time < 10 then
        "0" ++ String.fromInt time

    else
        String.fromInt time
