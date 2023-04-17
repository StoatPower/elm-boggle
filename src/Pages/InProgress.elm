module Pages.InProgress exposing (..)

import Element exposing (..)
import GameState exposing (GameState)
import Message exposing (Msg(..))
import UI.BoardView as BoardView
import UI.SelectionsView as SelectionsView
import UI.SubmissionsView as SubmissionsView
import UI.TimerView as TimerView


view : GameState -> Element Msg
view { board, selections, submissions, remainingSeconds } =
    column
        [ centerX
        , centerY
        , spacingXY 0 15
        ]
        [ TimerView.view remainingSeconds
        , el [ onLeft <| SubmissionsView.view submissions ] <|
            BoardView.view board
        , SelectionsView.view selections
        ]
