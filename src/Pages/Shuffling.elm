module Pages.Shuffling exposing (..)

import Board exposing (Board)
import Element exposing (..)
import Element.Font as Font
import Message exposing (Msg(..))
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
