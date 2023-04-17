module UI.PageView exposing (..)

import Element exposing (..)
import Game exposing (Game(..))
import Message exposing (Msg(..))
import Pages.GameOver
import Pages.HighScores
import Pages.InProgress
import Pages.Shuffling
import Pages.Unstarted
import UI.HeaderView as HeaderView



-- import UI.Palette exposing (..)


view : Game -> Element Msg
view game =
    column [ width fill, height fill ]
        [ HeaderView.view
        , el
            [ width fill
            , height fill
            ]
          <|
            case game of
                Unstarted wordsData ->
                    Pages.Unstarted.view wordsData

                Shuffling { board, submissions } ->
                    Pages.Shuffling.view board submissions

                InProgress gameState ->
                    Pages.InProgress.view gameState

                GameOver gameState ->
                    Pages.GameOver.view gameState

                HighScores { rounds } ->
                    Pages.HighScores.view rounds
        ]
