module Main exposing (..)

import Board exposing (Board, Grid)
import Browser exposing (Document)
import Cell exposing (Cell(..), XY)
import Die exposing (Die(..))
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import GameState exposing (GameState, Player, Rounds, Selections)
import List.Extra as LEx
import Palette exposing (..)
import Random
import RemoteData exposing (RemoteData(..), WebData)
import Scorebook exposing (Score, Scorebook, Word)
import Submissions exposing (Submission(..), Submissions)
import Time


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
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
    | DiceSwapped ( XY, XY )
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


lastTwoSelections : Selections -> ( Maybe Cell, Maybe Cell )
lastTwoSelections selections =
    case selections of
        [] ->
            ( Nothing, Nothing )

        first :: [] ->
            ( Just first, Nothing )

        first :: second :: _ ->
            ( Just first, Just second )


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
                            column [ centerX, centerY, spacingXY 0 15 ]
                                [ el
                                    [ Font.color atlantis
                                    , Font.size 34
                                    , centerX
                                    , Font.letterSpacing 2
                                    ]
                                  <|
                                    text " "
                                , boardView board
                                , submissionsView submissions
                                ]

                        InProgress { board, selections, submissions, remainingSeconds } ->
                            column
                                [ centerX
                                , centerY
                                , spacingXY 0 15
                                ]
                                [ timerView remainingSeconds
                                , el [ onLeft <| submissionsView submissions ] <|
                                    boardView board
                                , inputView selections
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
                        { attrs = [ Background.color atlantis ]
                        , onPress = Just ManuallyDownloadWords
                        , text = "Initialize Game"
                        }
                    ]

                Loading ->
                    [ el
                        [ centerX
                        , centerY
                        , Font.color chiffon
                        , Font.letterSpacing 2
                        , Font.size 22
                        ]
                      <|
                        text "Loading words dictionary..."
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
                        { attrs = [ Background.color atlantis ]
                        , onPress = Just ManuallyDownloadWords
                        , text = "Re-initialize Game?"
                        }
                    ]
    in
    column [ centerX, centerY ] content


inputView : Selections -> Element Msg
inputView selections =
    column
        [ width fill
        , Font.color atlantis
        , spacingXY 0 10
        ]
        [ selectionsView selections
        ]


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
            |> GameState.selectionsToWord
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
    column
        [ Font.size 18
        , Font.color atlantis
        , spacingXY 0 10
        , paddingXY 25 0
        , alignRight
        ]
        (row
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
            :: List.map submissionView submissions
        )


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
            viewFn word score [ Font.color alizarinCrimson ]


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
            , Font.color alizarinCrimson
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
        id =
            die
                |> Die.getId
                |> String.fromInt

        content =
            die
                |> Die.getFace
                |> Maybe.map text
                |> Maybe.withDefault none

        idView =
            el
                [ Font.size 8
                , alignLeft
                , alignTop
                , moveDown 5
                , moveRight 5
                ]
            <|
                text id
    in
    Input.button
        ([ height fill
         , width fill
         , centerX
         , centerY
         , Border.rounded 8
         , inFront idView
         ]
            ++ opts.attrs
        )
        { onPress = opts.onPress
        , label =
            el
                [ centerX
                , centerY
                ]
            <|
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
        Font.color alizarinCrimson

    else if score == 0 then
        Font.color chiffon

    else
        Font.color atlantis
