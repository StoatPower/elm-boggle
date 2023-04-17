module Message exposing (..)

import Game.Cell exposing (Cell(..), XY)
import Game.GameState exposing (Player)
import Game.Scorebook exposing (Score)
import RemoteData exposing (WebData)
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
