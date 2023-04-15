module Main exposing (..)

import Board exposing (Board, Grid)
import Browser exposing (Document)
import Cell exposing (Cell(..), XY)
import Color
import Dict exposing (Dict)
import Die exposing (Die(..))
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import List.Extra as LEx
import Random


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Player =
    String


type alias Rounds =
    List ( Player, Score )


type alias Score =
    Int


type alias Selections =
    List Cell


type alias Submission =
    ( String, Score )


type alias Submissions =
    List Submission


type alias GameState =
    { board : Board
    , selections : Selections
    , submissions : Submissions
    , rounds : Rounds
    }


initGameState : Board -> GameState
initGameState board =
    { board = board
    , selections = []
    , submissions = []
    , rounds = []
    }


type alias DieToShuffle =
    List XY


type Game
    = Unstarted
    | Shuffling GameState
    | InProgress GameState
    | GameOver GameState


type alias Selected =
    Bool


type alias Model =
    Game


init : ( Model, Cmd Msg )
init =
    ( Unstarted
    , Cmd.none
    )


type Msg
    = NoOp
    | ShuffleBoard
    | NewDieFace XY Int
    | SelectDie Cell
    | UnselectDie Cell
    | SubmitWord


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ShuffleBoard ->
            case model of
                Unstarted ->
                    let
                        gameState =
                            Board.init
                                |> Board.stageShuffle
                                |> initGameState
                    in
                    ( Shuffling gameState
                    , gameState.board
                        |> rollDiceCmds
                    )

                GameOver gameState ->
                    let
                        -- TODO : update gameState with new round results and reset other values
                        newBoard =
                            Board.stageShuffle gameState.board

                        newGameState =
                            { gameState
                                | board = newBoard
                                , selections = []
                                , submissions = []
                            }
                    in
                    ( Shuffling newGameState
                    , newGameState.board
                        |> rollDiceCmds
                    )

                _ ->
                    ( model, Cmd.none )

        NewDieFace key newFace ->
            case model of
                Shuffling gameState ->
                    let
                        newBoard =
                            gameState.board
                                |> Board.setNewDieFaceForCell key newFace

                        newGameState =
                            { gameState | board = newBoard }
                    in
                    if Board.isShuffling newBoard then
                        ( Shuffling newGameState, Cmd.none )

                    else
                        ( InProgress newGameState, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectDie cell ->
            case model of
                InProgress gameState ->
                    let
                        lastSelected =
                            List.head gameState.selections

                        newBoard =
                            gameState.board
                                |> Board.makeNewSelection
                                    cell
                                    lastSelected

                        newSelections =
                            cell :: gameState.selections

                        newGameState =
                            { gameState | board = newBoard, selections = newSelections }
                    in
                    ( InProgress newGameState, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UnselectDie _ ->
            -- todo
            ( model, Cmd.none )

        SubmitWord ->
            case model of
                InProgress gameState ->
                    let
                        newBoard =
                            Board.clearSelectedCells gameState.board

                        newSubmissions =
                            submitSelections gameState.selections
                                :: gameState.submissions

                        newGameState =
                            { gameState
                                | board = newBoard
                                , selections = []
                                , submissions = newSubmissions
                            }
                    in
                    ( InProgress newGameState, Cmd.none )

                _ ->
                    ( model, Cmd.none )


rollDiceCmds : Board -> Cmd Msg
rollDiceCmds board =
    board
        |> Dict.values
        |> List.map
            (\cell ->
                let
                    cmd =
                        NewDieFace <| Cell.getKey cell
                in
                Random.generate cmd Die.roll
            )
        |> Cmd.batch


selectionsToWord : Selections -> String
selectionsToWord selections =
    selections
        |> List.reverse
        |> List.filterMap Cell.getDieFace
        |> String.join ""


submitSelections : Selections -> Submission
submitSelections selections =
    -- todo - actually score the submission
    ( selectionsToWord selections
    , 0
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "Boggle"
    , body =
        [ El.layout [ height fill, width fill ] <|
            column [ width fill, height fill ]
                [ row [ width fill, height <| px 50 ]
                    [ el [ centerX, centerY ] <| text "Boggle"
                    ]
                , el [ width fill, height fill, grayBg ] <|
                    case model of
                        Unstarted ->
                            unstartedView

                        Shuffling gameState ->
                            el [ centerX, centerY ] <| text "Shuffling"

                        InProgress { board, selections, submissions } ->
                            column [ centerX, centerY ]
                                [ boardView board
                                , submitWordBtn
                                , selectionsView selections
                                , submissionsView submissions
                                ]

                        GameOver gameState ->
                            el [ centerX, centerY ] <| text "Game Over"
                ]
        ]
    }


unstartedView : Element Msg
unstartedView =
    el [ centerX, centerY ] <|
        Input.button []
            { onPress = Just ShuffleBoard
            , label = text "Start Game!"
            }


submitWordBtn : Element Msg
submitWordBtn =
    Input.button []
        { onPress = Just SubmitWord
        , label = text "Submit"
        }


selectionsView : Selections -> Element Msg
selectionsView selections =
    selections
        |> selectionsToWord
        |> text
        |> el [ width <| px 500, centerX ]


submissionsView : Submissions -> Element Msg
submissionsView submissions =
    submissions
        |> List.map submissionView
        |> column [ width fill ]


submissionView : Submission -> Element Msg
submissionView ( word, score ) =
    row []
        [ el [] <| text word
        , el [] <| text <| String.fromInt score
        ]


boardView : Board -> Element Msg
boardView board =
    board
        |> Board.toGrid
        |> gridView


gridView : Grid -> Element Msg
gridView grid =
    grid
        |> List.map rowView
        |> column [ grayBg ]


rowView : List Cell -> Element Msg
rowView cells =
    cells
        |> List.map cellView
        |> row [ grayBg, paddingXY 0 5, spacingXY 10 0 ]


cellView : Cell -> Element Msg
cellView cell =
    let
        ( bg, click, currentDie ) =
            case cell of
                Cell ( x, y ) die ->
                    ( whiteBg, SelectDie cell, die )

                SelectedCell ( x, y ) die ->
                    ( darkGrayBg, NoOp, die )

                CurrentCell ( x, y ) die ->
                    ( redBg, UnselectDie cell, die )

                RollingDieCell ( x, y ) die ->
                    ( darkGrayBg, NoOp, die )
    in
    el
        [ width <| px 100
        , height <| px 100
        , bg
        , onClick click
        ]
    <|
        dieView currentDie


dieView : Die -> Element Msg
dieView die =
    let
        content =
            die
                |> Die.getFace
                |> Maybe.map text
                |> Maybe.withDefault none
    in
    el [ height <| px 95, width <| px 95 ] <|
        el [ centerX, centerY ] <|
            content


grayBg : Attr decorative Msg
grayBg =
    bgColor Color.lightGray


darkGrayBg : Attr decorative Msg
darkGrayBg =
    bgColor Color.darkGray


redBg : Attr decorative Msg
redBg =
    bgColor Color.red


whiteBg : Attr decorative Msg
whiteBg =
    bgColor Color.white


bgColor : Color.Color -> Attr decorative Msg
bgColor color =
    color
        |> Color.toRgba
        |> fromRgb
        |> Background.color
