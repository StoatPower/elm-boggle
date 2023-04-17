module Message exposing (..)

import Cell exposing (Cell(..), XY)
import GameState exposing (Player)
import RemoteData exposing (WebData)
import Scorebook exposing (Score)
import Time


type Msg
    = NoOp
    | ManuallyDownloadWords
    | ScorebookSourceResponse (WebData String)
    | ShuffleBoard
    | DiceSwapped ( XY, XY )
    | NewDieFace XY Int
    | SelectDie Cell
    | UnselectDie
    | SubmitWord
    | PlayerInputChange String
    | SubmitPlayerScore Player Score
    | Tick Time.Posix
