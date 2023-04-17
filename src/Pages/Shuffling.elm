module Pages.Shuffling exposing (..)

import Board exposing (Board)
import Cell exposing (Cell(..))
import Die exposing (Die(..))
import Element exposing (..)
import Element.Font as Font
import Game exposing (Game(..))
import Message exposing (Msg(..))
import RemoteData exposing (RemoteData(..))
import Submissions exposing (Submission(..), Submissions)
import UI.BoardView as BoardView
import UI.Palette exposing (..)
import UI.SubmissionsView as SubmissionsView


view : Board -> Submissions -> Element Msg
view board submissions =
    column [ centerX, centerY, spacingXY 0 15 ]
        [ el
            [ Font.color atlantis
            , Font.size 34
            , centerX
            , Font.letterSpacing 2
            ]
          <|
            text " "
        , BoardView.view board
        , SubmissionsView.view submissions
        ]
