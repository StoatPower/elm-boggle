module Pages.Shuffling exposing (..)

import Element exposing (..)
import Element.Font as Font
import Game.Board exposing (Board)
import Game.Submissions exposing (Submission(..), Submissions)
import Message exposing (Msg(..))
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
        , el [ onLeft <| SubmissionsView.view submissions ] <|
            BoardView.view board
        , el [ height <| px 32, width fill ] none
        ]
