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
import Scorebook exposing (Score, Scorebook, Word)
import Submissions exposing (Submission(..), Submissions)
import Task
import Time


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
    , player : Maybe Player
    , rounds : Rounds
    , scorebook : Scorebook
    , elapsedSeconds : Int
    , allowedSeconds : Int
    }


initGameState : Scorebook -> Board -> GameState
initGameState scorebook board =
    { board = board
    , selections = []
    , submissions = []
    , player = Nothing
    , rounds = []
    , scorebook = scorebook
    , elapsedSeconds = 0
    , allowedSeconds = 180
    }


type Game
    = Unstarted (WebData Scorebook)
    | Shuffling GameState
    | InProgress GameState
    | GameOver GameState
    | HighScores GameState


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
    | PlayerInputChange String
    | SubmitPlayerScore Player Score
    | Tick Time.Posix


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

                HighScores gameState ->
                    let
                        newBoard =
                            Board.stageShuffle gameState.board

                        newGameState =
                            { gameState
                                | board = newBoard
                                , selections = []
                                , submissions = []
                                , elapsedSeconds = 0
                                , player = Nothing
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
                            Board.resetAllCells gameState.board

                        newSubmissions =
                            gameState.submissions
                                |> Submissions.submitWord
                                    (selectionsToWord gameState.selections)
                                    gameState.scorebook

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

        PlayerInputChange name ->
            case model of
                GameOver gameState ->
                    ( GameOver { gameState | player = Just name }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitPlayerScore player finalScore ->
            case model of
                GameOver gameState ->
                    let
                        newRounds =
                            ( player, finalScore ) :: gameState.rounds
                    in
                    ( HighScores { gameState | rounds = newRounds }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Tick _ ->
            case model of
                InProgress gameState ->
                    if gameState.elapsedSeconds < gameState.allowedSeconds then
                        ( InProgress
                            { gameState | elapsedSeconds = gameState.elapsedSeconds + 1 }
                        , Cmd.none
                        )

                    else
                        ( GameOver gameState, Cmd.none )

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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        InProgress { elapsedSeconds, allowedSeconds } ->
            if elapsedSeconds <= allowedSeconds then
                Time.every 1000 Tick

            else
                Sub.none

        _ ->
            Sub.none


view : Model -> Document Msg
view model =
    { title = "Boggle"
    , body =
        [ El.layout [ width fill, height fill, Background.color nero ] <|
            column [ width fill, height fill ]
                [ headerView
                , el
                    [ width fill
                    , height fill
                    ]
                  <|
                    case model of
                        Unstarted wordsData ->
                            unstartedView wordsData

                        Shuffling { board, submissions } ->
                            column [ centerX, centerY ]
                                [ boardView board

                                -- , submissionsView submissions
                                ]

                        InProgress { board, selections, submissions, elapsedSeconds } ->
                            column
                                [ centerX
                                , centerY
                                , spacingXY 0 15
                                ]
                                [ timerView elapsedSeconds
                                , el [ onLeft <| submissionsView submissions ] <|
                                    boardView board
                                , inputView model
                                ]

                        GameOver gameState ->
                            gameOverView gameState

                        HighScores gameState ->
                            highScoresView gameState.rounds
                ]
        ]
    }


headerView : Element Msg
headerView =
    row [ width fill, height <| px 50, Background.color atlantis ]
        [ el
            [ centerX
            , centerY
            , Font.color lisbonBrown
            , Font.letterSpacing 4
            , Font.size 24
            , Font.extraBold
            ]
          <|
            text "BOGGLE"
        ]


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
                        { attrs = [ Background.color atlantis ]
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


inputView : Model -> Element Msg
inputView model =
    case model of
        InProgress { selections } ->
            column
                [ width fill
                , Font.color atlantis
                , spacingXY 0 10
                ]
                [ selectionsView selections
                ]

        _ ->
            none


selectionsView : Selections -> Element Msg
selectionsView selections =
    row
        [ width <| px 500
        , paddingXY 0 5
        , centerX
        , Font.size 20
        , Border.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
        , Border.dotted
        , Border.color chiffon
        ]
        [ el [ width <| px 65 ] <| text ">>"
        , selections
            |> selectionsToWord
            |> text
            |> el [ Font.letterSpacing 4, paddingXY 10 0, centerX ]
        , el [ alignRight ] <| submitWordBtn selections
        ]


submissionsView : Submissions -> Element Msg
submissionsView submissions =
    let
        totalScore =
            submissions
                |> Submissions.tallySubmissions
    in
    submissions
        |> List.map submissionView
        |> List.append
            [ row
                [ Font.size 28
                , spacingXY 10 0
                , paddingEach { bottom = 5, top = 0, left = 0, right = 0 }
                , alignRight
                , Font.alignRight
                , Border.dotted
                , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
                , Border.color chiffon
                ]
                [ el [ Font.color chiffon, Font.letterSpacing 1.5 ] <| text "Total Score"
                , el [ scoreColor totalScore ] <|
                    el [ width <| px 45 ] <|
                        text <|
                            Scorebook.fmtScore totalScore
                ]
            ]
        |> column
            [ Font.size 18
            , Font.color atlantis
            , spacingXY 0 10
            , paddingXY 25 0
            , alignRight
            ]


submissionView : Submission -> Element Msg
submissionView submission =
    let
        viewFn word score scoreAttrs =
            row [ spacingXY 10 0, alignRight, Font.alignRight ]
                [ el [ Font.color chiffon, Font.letterSpacing 1.5 ] <|
                    text word
                , el ([] ++ scoreAttrs) <|
                    el [ width <| px 45 ] <|
                        text <|
                            Scorebook.fmtScore score
                ]
    in
    case submission of
        ValidWord word score ->
            viewFn word score []

        InvalidWord word score ->
            viewFn word score [ Font.color red ]


gameOverView : GameState -> Element Msg
gameOverView gameState =
    let
        finalScore =
            gameState.submissions
                |> Submissions.tallySubmissions
    in
    column
        [ centerX
        , centerY
        , spacingXY 0 40
        ]
        [ el
            [ Font.bold
            , Font.size 64
            , Font.color red
            , Font.letterSpacing 8
            ]
          <|
            text "GAME OVER"
        , row
            [ centerX
            , Font.size 24
            , Font.letterSpacing 2
            , spacingXY 25 0
            , Border.dotted
            , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
            , Border.color chiffon
            , paddingXY 0 10
            ]
            [ el [ Font.color chiffon ] <| text "Final Score"
            , el [ scoreColor finalScore ] <| text (String.fromInt finalScore)
            ]
        , row
            [ centerX ]
            [ Input.text
                [ Font.center
                , Font.letterSpacing 1.5
                , Background.color chiffon
                , Font.color lisbonBrown
                , Border.roundEach { topLeft = 5, bottomLeft = 5, topRight = 0, bottomRight = 0 }
                ]
                { onChange = PlayerInputChange
                , text = Maybe.withDefault "" gameState.player
                , placeholder = Just <| Input.placeholder [ Font.center ] <| text "Enter Name"
                , label = Input.labelHidden "Enter Name"
                }
            , Input.button
                [ Background.color atlantis
                , Font.color lisbonBrown
                , Font.letterSpacing 1.5
                , height fill
                , Border.roundEach { topLeft = 0, bottomLeft = 0, topRight = 5, bottomRight = 5 }
                ]
                { onPress =
                    case gameState.player of
                        Just player ->
                            let
                                nameLength =
                                    player |> String.trim |> String.length
                            in
                            if nameLength > 0 then
                                Just <| SubmitPlayerScore player finalScore

                            else
                                Nothing

                        Nothing ->
                            Nothing
                , label = el [ paddingXY 10 0 ] <| text "Submit"
                }
            ]
        ]


highScoresView : Rounds -> Element Msg
highScoresView rounds =
    let
        top10 =
            rounds
                |> List.sortBy Tuple.second
                |> List.take 10
                |> List.reverse
    in
    column
        [ centerX
        , centerY
        , width <| px 500
        , Font.color chiffon
        , Font.letterSpacing 1.5
        , spacingXY 0 25
        ]
        [ el
            [ Font.size 24
            , Font.bold
            , Font.center
            , Font.letterSpacing 2
            , width fill
            , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
            , Border.dotted
            , paddingXY 0 10
            ]
          <|
            text "HIGH SCORES"
        , top10
            |> List.indexedMap highScoreView
            |> column [ width fill, spacingXY 0 25 ]
        , standardBtn
            { attrs =
                [ Background.color atlantis
                , centerX
                , Font.color lisbonBrown
                , moveDown 100
                ]
            , onPress = Just ShuffleBoard
            , text = "Next Round!"
            }
        ]


highScoreView : Int -> ( Player, Score ) -> Element Msg
highScoreView index ( player, score ) =
    row
        [ width fill
        , Font.color chiffon
        , Font.size 18
        , Font.letterSpacing 1.5
        , spacingXY 15 0
        ]
        [ el [] <| text (padNumber (index + 1) ++ ".")
        , el [] <| text player
        , el [ scoreColor score, alignRight ] <| text <| String.fromInt score
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
        |> column [ Background.color chiffon ]


rowView : List Cell -> Element Msg
rowView cells =
    cells
        |> List.map cellView
        |> row [ Background.color nero, paddingXY 0 5, spacingXY 10 0 ]


cellView : Cell -> Element Msg
cellView cell =
    let
        ( cellAttrs, dieOpts, currentDie ) =
            case cell of
                Cell ( x, y ) die ->
                    ( []
                    , { attrs = [ Background.color shadowGreen ]
                      , onPress = Just (SelectDie cell)
                      }
                    , die
                    )

                AvailableCell ( x, y ) die ->
                    ( []
                    , { attrs = [ Background.color shadowGreen ]
                      , onPress = Just <| SelectDie cell
                      }
                    , die
                    )

                UnreachableCell ( x, y ) die ->
                    ( []
                    , { attrs = [ Background.color shadowGreen ]
                      , onPress = Nothing
                      }
                    , die
                    )

                SelectedCell ( x, y ) die ->
                    ( []
                    , { attrs = [ Background.color lisbonBrown, Font.color atlantis ]
                      , onPress = Nothing
                      }
                    , die
                    )

                CurrentCell ( x, y ) die ->
                    ( []
                    , { attrs = [ Background.color <| atlantis, Font.color lisbonBrown ]
                      , onPress = Just UnselectDie
                      }
                    , die
                    )

                RollingDieCell ( x, y ) die ->
                    ( []
                    , { attrs = [ Background.color shadowGreen ], onPress = Nothing }
                    , die
                    )
    in
    el
        ([ width <| px 100
         , height <| px 100
         , paddingXY 10 10
         , Border.rounded 4
         , Background.color chiffon
         ]
            ++ cellAttrs
        )
    <|
        dieView
            dieOpts
            currentDie


dieView : { opts | attrs : List (Attr () Msg), onPress : Maybe Msg } -> Die -> Element Msg
dieView opts die =
    let
        content =
            die
                |> Die.getFace
                |> Maybe.map text
                |> Maybe.withDefault none
    in
    Input.button
        ([ height fill
         , width fill
         , centerX
         , centerY
         , Border.rounded 8
         ]
            ++ opts.attrs
        )
        { onPress = opts.onPress
        , label =
            el [ centerX, centerY ] <|
                content
        }


submitWordBtn : Selections -> Element Msg
submitWordBtn selections =
    let
        ( attrs, msg ) =
            if List.length selections >= Scorebook.minWordLength then
                ( [ Font.color atlantis ], Just SubmitWord )

            else
                ( [ Font.color chiffon ], Nothing )
    in
    Input.button
        ([ width <| px 65 ] ++ attrs)
        { onPress = msg
        , label = el [ centerX, centerY, Font.letterSpacing 1.5 ] <| text "Submit"
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
         , height <| px 55
         , Border.rounded 5
         , Font.letterSpacing 2
         ]
            ++ opts.attrs
        )
        { onPress = opts.onPress
        , label = el [ centerX, centerY ] <| text opts.text
        }


timerView : Int -> Element Msg
timerView elapsedSeconds =
    let
        minutes =
            padNumber <| elapsedSeconds // 60

        seconds =
            elapsedSeconds
                |> modBy 60
                |> padNumber
    in
    el [ Font.color atlantis, Font.size 34, centerX ] <|
        text (minutes ++ ":" ++ seconds)


padNumber : Int -> String
padNumber time =
    if time < 10 then
        "0" ++ String.fromInt time

    else
        String.fromInt time


scoreColor : Score -> Attr () Msg
scoreColor score =
    if score < 0 then
        Font.color red

    else if score == 0 then
        Font.color chiffon

    else
        Font.color atlantis
