module UI.LayoutView exposing (..)

import Cell exposing (Cell(..))
import Die exposing (Die(..))
import Element exposing (..)
import Element.Background as Background
import Game exposing (Game(..))
import Html exposing (Html)
import Message exposing (Msg(..))
import RemoteData exposing (RemoteData(..))
import Submissions exposing (Submission(..))
import UI.PageView as PageView
import UI.Palette exposing (..)


view : Game -> Html Msg
view game =
    layout [ width fill, height fill, Background.color nero ] <|
        PageView.view game
