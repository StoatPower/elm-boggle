module Game exposing (..)

import GameState exposing (GameState)
import RemoteData exposing (WebData)
import Scorebook exposing (Scorebook)


type Game
    = Unstarted (WebData Scorebook)
    | Shuffling GameState
    | InProgress GameState
    | GameOver GameState
    | HighScores GameState
