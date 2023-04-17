module Main exposing (..)

import Browser exposing (Document)
import Game exposing (Game(..))
import Message exposing (Msg(..))
import RemoteData exposing (RemoteData(..))
import UI.LayoutView as LayoutView


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
    , Game.getScorebookSource ScorebookSourceResponse
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ManuallyDownloadWords ->
            Game.manuallyDownloadWords model

        ScorebookSourceResponse response ->
            Game.buildScorebook response model

        ShuffleBoard ->
            Game.shuffleBoard model

        NewDieFace key newFace ->
            Game.handleDiceRoll key newFace model

        DiceSwapped ( xy1, xy2 ) ->
            Game.handleDiceSwap xy1 xy2 model

        SelectDie cell ->
            Game.selectNewDie cell model

        UnselectDie ->
            Game.unselectCurrentDie model

        SubmitWord ->
            Game.submitWord model

        PlayerInputChange name ->
            Game.handlePlayerInput name model

        SubmitPlayerScore player finalScore ->
            Game.submitPlayerScore player finalScore model

        Tick _ ->
            Game.handleTimerIncrement model


subscriptions : Model -> Sub Msg
subscriptions model =
    Game.subscriptions model


view : Model -> Document Msg
view model =
    { title = "Boggle"
    , body =
        [ LayoutView.view model
        ]
    }
