module Pages.InProgress exposing (..)

import Cell exposing (Cell(..))
import Die exposing (Die(..))
import Element exposing (..)
import Game exposing (Game(..))
import GameState exposing (GameState)
import Message exposing (Msg(..))
import RemoteData exposing (RemoteData(..))
import Submissions exposing (Submission(..))
import UI.BoardView as BoardView
import UI.Palette exposing (..)
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
