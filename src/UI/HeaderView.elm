module UI.HeaderView exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Message exposing (Msg(..))
import UI.Palette exposing (..)


view : Element Msg
view =
    row [ width fill, height <| px 50, Background.color atlantis ]
        [ el
            [ centerX
            , centerY
            , Font.color lisbonBrown
            , Font.letterSpacing 4
            , Font.size 24
            , Font.extraBold
            ]
          <|
            text "BOGGLE"
        ]
