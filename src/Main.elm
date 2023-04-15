module Main exposing (..)

import Board exposing (Board, Grid)
import Browser exposing (Document)
import Cell exposing (Cell(..), XY)
import Dict exposing (Dict)
import Die exposing (Die(..))
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import List.Extra as LEx
import Palette exposing (..)
import Random
import RemoteData exposing (RemoteData(..), WebData)
import Scorebook exposing (Score, Scorebook, Submission(..), Submissions, Word)


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


type alias Selections =
    List Cell


type alias GameState =
    { board : Board
    , selections : Selections
    , submissions : Submissions
    , rounds : Rounds
    , scorebook : Scorebook
    }


initGameState : Scorebook -> Board -> GameState
initGameState scorebook board =
    { board = board
    , selections = []
    , submissions = []
    , rounds = []
    , scorebook = scorebook
    }


type Game
    = Unstarted (WebData Scorebook)
    | Shuffling GameState
    | InProgress GameState
    | GameOver GameState


type alias Model =
    Game


init : ( Model, Cmd Msg )
init =
    ( Unstarted Loading
    , Scorebook.getScorebookSource ScorebookSourceResponse
    )


type Msg
    = NoOp
    | ManuallyDownloadWords
    | ScorebookSourceResponse (WebData String)
    | ShuffleBoard
    | NewDieFace XY Int
    | SelectDie Cell
    | UnselectDie
    | SubmitWord


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
            let
                scorebookData =
                    sbData
                        |> RemoteData.map Scorebook.buildScorebook
            in
            case model of
                Unstarted _ ->
                    ( Unstarted scorebookData, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ShuffleBoard ->
            case model of
                Unstarted (Success words) ->
                    let
                        gameState =
                            Board.init
                                |> Board.stageShuffle
                                |> initGameState words
                    in
                    ( Shuffling gameState
                    , gameState.board
                        |> rollDiceCmds
                    )

                Unstarted _ ->
                    ( model, Cmd.none )

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

        UnselectDie ->
            case model of
                InProgress gameState ->
                    let
                        ( newBoard, newSelections ) =
                            case gameState.selections of
                                [] ->
                                    ( gameState.board, [] )

                                h :: [] ->
                                    ( gameState.board
                                        |> Board.undoLastSelection h Nothing
                                    , []
                                    )

                                h :: n :: t ->
                                    ( gameState.board
                                        |> Board.undoLastSelection h (Just n)
                                    , n :: t
                                    )

                        newGameState =
                            { gameState | board = newBoard, selections = newSelections }
                    in
                    ( InProgress newGameState, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitWord ->
            case model of
                InProgress gameState ->
                    let
                        newBoard =
                            Board.clearSelectedCells gameState.board

                        newSubmissions =
                            submitSelections gameState.selections gameState.scorebook
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


lastTwoSelections : Selections -> ( Maybe Cell, Maybe Cell )
lastTwoSelections selections =
    case selections of
        [] ->
            ( Nothing, Nothing )

        first :: [] ->
            ( Just first, Nothing )

        first :: second :: _ ->
            ( Just first, Just second )


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


selectionsToWord : Selections -> Word
selectionsToWord selections =
    selections
        |> List.reverse
        |> List.filterMap Cell.getDieFace
        |> String.join ""


submitSelections : Selections -> Scorebook -> Submission
submitSelections selections scorebook =
    scorebook
        |> Scorebook.submitWord (selectionsToWord selections)


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
                , el [ width fill, height fill, Background.color gray ] <|
                    case model of
                        Unstarted wordsData ->
                            unstartedView wordsData

                        Shuffling { board, submissions } ->
                            column [ centerX, centerY ]
                                [ boardView board
                                , submissionsView submissions
                                ]

                        InProgress { board, selections, submissions } ->
                            column [ centerX, centerY ]
                                [ boardView board
                                , submitWordBtn selections
                                , selectionsView selections
                                , submissionsView submissions
                                ]

                        GameOver gameState ->
                            el [ centerX, centerY ] <| text "Game Over"
                ]
        ]
    }


unstartedView : WebData Scorebook -> Element Msg
unstartedView wordsData =
    let
        content =
            case wordsData of
                NotAsked ->
                    [ standardBtn
                        { attrs = [ Background.color darkGray ]
                        , onPress = Just ManuallyDownloadWords
                        , text = "Initialize Game"
                        }
                    ]

                Loading ->
                    [ text "Loading words dictionary..."
                    ]

                Success _ ->
                    [ standardBtn
                        { attrs = [ Background.color darkGray ]
                        , onPress = Just ShuffleBoard
                        , text = "Start Game!"
                        }
                    ]

                Failure _ ->
                    [ text "Failed to load words dictionary :("
                    , standardBtn
                        { attrs = [ Background.color darkGray ]
                        , onPress = Just ManuallyDownloadWords
                        , text = "Re-initialize Game?"
                        }
                    ]
    in
    column [ centerX, centerY ] content


submitWordBtn : Selections -> Element Msg
submitWordBtn selections =
    let
        ( attrs, msg ) =
            if List.length selections >= Scorebook.minWordLength then
                ( [ Background.color green ], Just SubmitWord )

            else
                ( [ Background.color darkGray ], Nothing )
    in
    standardBtn
        { attrs = attrs
        , onPress = msg
        , text = "Submit"
        }


standardBtn :
    { opts
        | attrs : List (Attr () Msg)
        , onPress : Maybe Msg
        , text : String
    }
    -> Element Msg
standardBtn opts =
    Input.button
        ([ width <| px 250
         , height <| px 65
         , Border.rounded 5
         ]
            ++ opts.attrs
        )
        { onPress = opts.onPress
        , label = el [ centerX, centerY ] <| text opts.text
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
submissionView submission =
    case submission of
        ValidWord ( word, score ) ->
            row []
                [ el [] <| text word
                , el [] <| text <| String.fromInt score
                ]

        InvalidWord ( word, score ) ->
            row []
                [ el [] <| text word
                , el [ Font.color red ] <| text <| String.fromInt score
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
        |> column [ Background.color gray ]


rowView : List Cell -> Element Msg
rowView cells =
    cells
        |> List.map cellView
        |> row [ Background.color gray, paddingXY 0 5, spacingXY 10 0 ]


cellView : Cell -> Element Msg
cellView cell =
    let
        ( bg, click, currentDie ) =
            case cell of
                Cell ( x, y ) die ->
                    ( Background.color white, SelectDie cell, die )

                SelectedCell ( x, y ) die ->
                    ( Background.color darkGray, NoOp, die )

                CurrentCell ( x, y ) die ->
                    ( Background.color red, UnselectDie, die )

                RollingDieCell ( x, y ) die ->
                    ( Background.color darkGray, NoOp, die )
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
