module Main exposing (..)

import Board exposing (..)
import Browser exposing (Document)
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



-- dimensions : Int
-- dimensions =
--     5


type alias Model =
    Game


init : ( Model, Cmd Msg )
init =
    ( Unstarted
    , Cmd.none
    )


type Msg
    = NoOp
    | StartGame
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

        StartGame ->
            ( InProgress initGameState, Cmd.none )

        ShuffleBoard ->
            case model of
                InProgress ({ board } as state) ->
                    let
                        ( keys, values ) =
                            board
                                |> Dict.foldl
                                    (\( k, v ) ( ks, vs ) ->
                                        ( k :: ks, v :: vs )
                                    )
                                    ( [], [] )
                    in
                    ( Shuffling state keys
                    , values
                        |> List.map
                            (\(Cell ( x, y ) _) ->
                                Random.generate (NewDieFace ( x, y )) Die.roll
                            )
                        |> Cmd.batch
                    )

                _ ->
                    ( model, Cmd.none )

        NewDieFace key newIndex ->
            case model.state of
                InProgress board score rounds selections ->
                    let
                        newBoard =
                            board
                                |> updateBoardDie key newIndex
                    in
                    ( { model | state = InProgress newBoard score rounds selections }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectDie ((Cell key _ _) as cell) ->
            case model.state of
                InProgress board score rounds (current :: selections) ->
                    let
                        newCurrent =
                            cell :: current

                        newSelections =
                            newCurrent :: selections

                        newBoard =
                            board
                                |> selectBoardCell key

                        newState =
                            InProgress newBoard score rounds newSelections
                    in
                    ( { model | state = newState }, Cmd.none )

                InProgress board score rounds [] ->
                    let
                        newSelections =
                            [ [ cell ] ]

                        newBoard =
                            board
                                |> selectBoardCell key

                        newState =
                            InProgress newBoard score rounds newSelections
                    in
                    ( { model | state = newState }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UnselectDie _ ->
            -- todo
            ( model, Cmd.none )

        SubmitWord ->
            case model.state of
                InProgress board score rounds selections ->
                    let
                        newBoard =
                            clearSelections board

                        newState =
                            InProgress newBoard score rounds ([] :: selections)
                    in
                    ( { model | state = newState }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


updateBoardDie : XY -> Int -> Board -> Board
updateBoardDie key newIndex board =
    board
        |> Dict.update key (updateCellDie newIndex)


updateCellDie : Int -> Maybe Cell -> Maybe Cell
updateCellDie newIndex maybeCell =
    case maybeCell of
        Just (Cell ( x, y ) (Die _ dieConfig) selected) ->
            Just <| Cell ( x, y ) (Die newIndex dieConfig) selected

        Nothing ->
            Nothing


selectBoardCell : XY -> Board -> Board
selectBoardCell key board =
    board
        |> Dict.update key selectCell


clearSelections : Board -> Board
clearSelections board =
    board
        |> Dict.map (\k (Cell key die _) -> Cell key die False)


selectCell : Maybe Cell -> Maybe Cell
selectCell maybeCell =
    case maybeCell of
        Just (Cell ( x, y ) die _) ->
            Just <| Cell ( x, y ) die True

        Nothing ->
            Nothing


unselectCell : Maybe Cell -> Maybe Cell
unselectCell maybeCell =
    case maybeCell of
        Just (Cell ( x, y ) die _) ->
            Just <| Cell ( x, y ) die False

        Nothing ->
            Nothing


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
                    case model.state of
                        Unstarted ->
                            unstartedView

                        InProgress board score rounds selections ->
                            column [ centerX, centerY ]
                                [ boardView board selections
                                , submitWordBtn
                                , selectionsView selections
                                ]

                        GameOver score rounds ->
                            none
                ]
        ]
    }


unstartedView : Element Msg
unstartedView =
    el [ centerX, centerY ] <|
        Input.button []
            { onPress = Just StartGame
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
        |> List.map selectionView
        |> column [ width <| px 500, centerX ]


selectionView : List Cell -> Element Msg
selectionView stack =
    stack
        |> List.reverse
        |> List.filterMap
            (\(Cell _ die _) ->
                Die.getFace die
            )
        |> String.join ""
        |> text
        |> el [ width fill ]


boardView : Board -> Selections -> Element Msg
boardView board selections =
    board
        |> boardToGrid
        |> gridView selections


gridView : Selections -> Grid -> Element Msg
gridView selections grid =
    grid
        |> List.map (rowView selections)
        |> column [ grayBg ]


rowView : Selections -> List Cell -> Element Msg
rowView selections cells =
    let
        current =
            case selections of
                currentWord :: _ ->
                    case currentWord of
                        (Cell ( x, y ) _ _) :: _ ->
                            Just ( x, y )

                        [] ->
                            Nothing

                [] ->
                    Nothing
    in
    cells
        |> List.map
            (\((Cell key _ _) as cell) ->
                if Just key == current then
                    currentCellView cell

                else
                    cellView cell
            )
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
                    ( redBg, UnselectDie, die )

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



-- TYPES AND STUFF


type alias Rounds =
    Int


type alias Score =
    Int


type alias Selections =
    List (List Cell)


type alias GameState =
    { board : Board
    , score : Score
    , rounds : Rounds
    , selections : Selections
    }


initGameState : GameState
initGameState =
    { board = initBoard
    , score = 0
    , rounds = 0
    , selections = []
    }


type alias GameResult =
    { playerName : Maybe String
    , score : Score
    , rounds : Rounds
    }


type alias DieToShuffle =
    List XY


type Game
    = Unstarted
    | Shuffling GameState DieToShuffle
    | InProgress GameState
    | GameOver GameResult


type alias Selected =
    Bool
