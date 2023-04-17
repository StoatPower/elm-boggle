module Main exposing (..)

import Board exposing (Board)
import Browser exposing (Document)
import Cell exposing (Cell(..))
import Die exposing (Die(..))
import Element exposing (..)
import Game exposing (Game(..))
import GameState
import List.Extra as LEx
import Message exposing (Msg(..))
import Random
import RemoteData exposing (RemoteData(..))
import Scorebook
import Submissions exposing (Submission(..))
import Time
import UI.LayoutView as LayoutView
import UI.Palette exposing (..)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    Game


init : ( Model, Cmd Msg )
init =
    ( Unstarted Loading
    , Scorebook.getScorebookSource ScorebookSourceResponse
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ManuallyDownloadWords ->
            case model of
                Unstarted _ ->
                    ( Unstarted Loading, Scorebook.getScorebookSource ScorebookSourceResponse )

                _ ->
                    ( model, Cmd.none )

        ScorebookSourceResponse sbData ->
            case model of
                Unstarted _ ->
                    ( Unstarted <| RemoteData.map Scorebook.buildScorebook sbData, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ShuffleBoard ->
            case model of
                Unstarted (Success words) ->
                    let
                        gameState =
                            GameState.init words
                    in
                    ( Shuffling gameState
                    , gameState.board
                        |> rollDiceCmd
                    )

                Unstarted _ ->
                    ( model, Cmd.none )

                HighScores gameState ->
                    let
                        newGameState =
                            GameState.reInit gameState
                    in
                    ( Shuffling newGameState
                    , newGameState.board
                        |> rollDiceCmd
                    )

                _ ->
                    ( model, Cmd.none )

        NewDieFace key newFace ->
            case model of
                Shuffling gameState ->
                    ( Shuffling <| GameState.newDieFace key newFace gameState, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DiceSwapped ( xy1, xy2 ) ->
            case model of
                Shuffling gameState ->
                    ( Shuffling <| GameState.swapDice xy1 xy2 gameState, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectDie cell ->
            case model of
                InProgress gameState ->
                    ( InProgress <| GameState.selectDie cell gameState, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UnselectDie ->
            case model of
                InProgress gameState ->
                    ( InProgress <| GameState.unselectDie gameState, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitWord ->
            case model of
                InProgress gameState ->
                    ( InProgress <| GameState.submitWord gameState, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PlayerInputChange name ->
            case model of
                GameOver gameState ->
                    ( GameOver <| GameState.updatePlayer name gameState, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitPlayerScore player finalScore ->
            case model of
                GameOver gameState ->
                    ( HighScores <| GameState.submitPlayerScore player finalScore gameState
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Tick _ ->
            case model of
                Shuffling gameState ->
                    case GameState.countdownShuffleTimer gameState of
                        Just ( newGameState, True ) ->
                            ( Shuffling newGameState
                            , newGameState.board |> rollDiceCmd
                            )

                        Just ( newGameState, False ) ->
                            ( Shuffling newGameState, Cmd.none )

                        Nothing ->
                            ( InProgress gameState, Cmd.none )

                InProgress gameState ->
                    case GameState.countdownGameTimer gameState of
                        Just newGameState ->
                            ( InProgress newGameState, Cmd.none )

                        Nothing ->
                            ( GameOver gameState, Cmd.none )

                _ ->
                    ( model, Cmd.none )


rollDiceCmd : Board -> Cmd Msg
rollDiceCmd board =
    let
        newDieFaceCmds cells =
            cells
                |> List.map
                    (\cell ->
                        let
                            cmd =
                                NewDieFace <| Cell.getKey cell
                        in
                        Random.generate cmd Die.roll
                    )

        diceSwappedCmds =
            List.repeat (Board.size board * 2)
                (Random.generate DiceSwapped Board.randomSwap)
    in
    board
        |> Board.getCells
        |> newDieFaceCmds
        |> LEx.interweave diceSwappedCmds
        |> Cmd.batch


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Shuffling { remainingShuffleMillis } ->
            if remainingShuffleMillis >= 0 then
                Time.every 1 Tick

            else
                Sub.none

        InProgress { remainingSeconds } ->
            if remainingSeconds >= 0 then
                Time.every 1000 Tick

            else
                Sub.none

        _ ->
            Sub.none


view : Model -> Document Msg
view model =
    { title = "Boggle"
    , body =
        [ LayoutView.view model
        ]
    }
