module UI.LayoutView exposing (..)

import Element exposing (..)
import Element.Background as Background
import Game exposing (Game(..))
import Html exposing (Html)
import Message exposing (Msg(..))
import UI.PageView as PageView
import UI.Palette exposing (..)


view : Game -> Html Msg
view game =
    layout [ width fill, height fill, Background.color nero ] <|
        PageView.view game
