module Game exposing (..)

import Board
import Cell exposing (Cell, XY)
import Die exposing (DieConfigIdx)
import GameState exposing (GameState, Player)
import Http
import List.Extra as LEx
import Message exposing (Msg(..))
import Random
import RemoteData exposing (RemoteData(..), WebData)
import Scorebook exposing (Score, Scorebook)
import Time


type Game
    = Unstarted (WebData Scorebook)
    | Shuffling GameState
    | InProgress GameState
    | GameOver GameState
    | HighScores GameState


rollDice : GameState -> Cmd Msg
rollDice { board } =
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


getScorebookSource : (WebData String -> Msg) -> Cmd Msg
getScorebookSource message =
    Http.get
        { url = Scorebook.wordsUrl
        , expect = Http.expectString (RemoteData.fromResult >> message)
        }


manuallyDownloadWords : Game -> ( Game, Cmd Msg )
manuallyDownloadWords game =
    case game of
        Unstarted _ ->
            ( Unstarted Loading, getScorebookSource ScorebookSourceResponse )

        _ ->
            ( game, Cmd.none )


buildScorebook : WebData String -> Game -> ( Game, Cmd Msg )
buildScorebook response game =
    case game of
        Unstarted _ ->
            ( Unstarted <| RemoteData.map Scorebook.buildScorebook response, Cmd.none )

        _ ->
            ( game, Cmd.none )


shuffleBoard : Game -> ( Game, Cmd Msg )
shuffleBoard game =
    case game of
        Unstarted (Success words) ->
            let
                gameState =
                    GameState.init words
            in
            ( Shuffling gameState
            , gameState
                |> rollDice
            )

        Unstarted _ ->
            ( game, Cmd.none )

        HighScores gameState ->
            let
                newGameState =
                    GameState.reInit gameState
            in
            ( Shuffling newGameState
            , newGameState
                |> rollDice
            )

        _ ->
            ( game, Cmd.none )


handleDiceRoll : XY -> DieConfigIdx -> Game -> ( Game, Cmd Msg )
handleDiceRoll key newFace game =
    case game of
        Shuffling gameState ->
            ( Shuffling <| GameState.newDieFace key newFace gameState, Cmd.none )

        _ ->
            ( game, Cmd.none )


handleDiceSwap : XY -> XY -> Game -> ( Game, Cmd Msg )
handleDiceSwap xy1 xy2 game =
    case game of
        Shuffling gameState ->
            ( Shuffling <| GameState.swapDice xy1 xy2 gameState, Cmd.none )

        _ ->
            ( game, Cmd.none )


selectNewDie : Cell -> Game -> ( Game, Cmd Msg )
selectNewDie cell game =
    case game of
        InProgress gameState ->
            ( InProgress <| GameState.selectDie cell gameState, Cmd.none )

        _ ->
            ( game, Cmd.none )


unselectCurrentDie : Game -> ( Game, Cmd Msg )
unselectCurrentDie game =
    case game of
        InProgress gameState ->
            ( InProgress <| GameState.unselectDie gameState, Cmd.none )

        _ ->
            ( game, Cmd.none )


submitWord : Game -> ( Game, Cmd Msg )
submitWord game =
    case game of
        InProgress gameState ->
            ( InProgress <| GameState.submitWord gameState, Cmd.none )

        _ ->
            ( game, Cmd.none )


handlePlayerInput : String -> Game -> ( Game, Cmd Msg )
handlePlayerInput input game =
    case game of
        GameOver gameState ->
            ( GameOver <| GameState.updatePlayer input gameState, Cmd.none )

        _ ->
            ( game, Cmd.none )


submitPlayerScore : Player -> Score -> Game -> ( Game, Cmd Msg )
submitPlayerScore player score game =
    case game of
        GameOver gameState ->
            ( HighScores <| GameState.submitPlayerScore player score gameState
            , Cmd.none
            )

        _ ->
            ( game, Cmd.none )


handleTimerIncrement : Game -> ( Game, Cmd Msg )
handleTimerIncrement game =
    case game of
        Shuffling gameState ->
            case GameState.countdownShuffleTimer gameState of
                Just ( newGameState, True ) ->
                    ( Shuffling newGameState
                    , newGameState |> rollDice
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
            ( game, Cmd.none )


subscriptions : Game -> Sub Msg
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
