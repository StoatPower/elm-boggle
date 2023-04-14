module Main exposing (..)

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


dimensions : Int
dimensions =
    5


type alias Model =
    { state : GameState
    }


defaultModel : Model
defaultModel =
    Model Unstarted


init : ( Model, Cmd Msg )
init =
    ( defaultModel
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
            ( { model | state = InProgress initBoard 0 0 [] }, Cmd.none )

        ShuffleBoard ->
            case model.state of
                InProgress board score turns selections ->
                    ( model
                    , board
                        |> Dict.values
                        |> List.map
                            (\(Cell ( x, y ) die selected) ->
                                Random.generate (NewDieFace ( x, y )) Die.roll
                            )
                        |> Cmd.batch
                    )

                _ ->
                    ( model, Cmd.none )

        NewDieFace key newIndex ->
            case model.state of
                InProgress board score turns selections ->
                    let
                        newBoard =
                            board
                                |> updateBoardDie key newIndex
                    in
                    ( { model | state = InProgress newBoard score turns selections }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectDie ((Cell key _ _) as cell) ->
            case model.state of
                InProgress board score turns (current :: selections) ->
                    let
                        newCurrent =
                            cell :: current

                        newSelections =
                            newCurrent :: selections

                        newBoard =
                            board
                                |> selectBoardCell key

                        newState =
                            InProgress newBoard score turns newSelections
                    in
                    ( { model | state = newState }, Cmd.none )

                InProgress board score turns [] ->
                    let
                        newSelections =
                            [ [ cell ] ]

                        newBoard =
                            board
                                |> selectBoardCell key

                        newState =
                            InProgress newBoard score turns newSelections
                    in
                    ( { model | state = newState }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UnselectDie _ ->
            -- todo
            ( model, Cmd.none )

        SubmitWord ->
            case model.state of
                InProgress board score turns selections ->
                    let
                        newBoard =
                            clearSelections board

                        newState =
                            InProgress newBoard score turns ([] :: selections)
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

                        InProgress board score turns selections ->
                            column [ centerX, centerY ]
                                [ boardView board selections
                                , submitWordBtn
                                , selectionsView selections
                                ]

                        GameOver score turns ->
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



--     currentView =
--         currentCellView current
--     restView =
--         rest
--             |> List.map cellView
-- in
-- currentView
--     :: restView
--     |> row [ grayBg, paddingXY 0 5, spacingXY 10 0 ]


currentCellView : Cell -> Element Msg
currentCellView ((Cell _ die selected) as cell) =
    let
        ( bg, click ) =
            if selected then
                ( redBg, UnselectDie cell )

            else
                ( whiteBg, NoOp )
    in
    el
        [ width <| px 100
        , height <| px 100
        , bg
        , onClick click
        ]
    <|
        dieView die


cellView : Cell -> Element Msg
cellView ((Cell _ die selected) as cell) =
    let
        ( bg, click ) =
            if selected then
                ( darkGrayBg, NoOp )

            else
                ( whiteBg, SelectDie cell )
    in
    el
        [ width <| px 100
        , height <| px 100
        , bg
        , onClick click
        ]
    <|
        dieView die


dieView : Die -> Element Msg
dieView die =
    let
        content =
            die
                |> Die.getFace
                |> Maybe.map text
                |> Maybe.withDefault none
    in
    el [ centerX, centerY ] content


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


type alias Turns =
    Int


type alias Score =
    Int


type alias Selections =
    List (List Cell)


type GameState
    = Unstarted
    | InProgress Board Score Turns Selections
    | GameOver Score Turns


type alias XY =
    ( Int, Int )


type alias Selected =
    Bool


type Cell
    = Cell XY Die Selected


type alias Board =
    Dict XY Cell


type alias Grid =
    List (List Cell)


initBoard : Board
initBoard =
    let
        range =
            List.range 0 (dimensions - 1)

        dice =
            Die.defaultDice
    in
    range
        |> List.concatMap
            (\x -> List.map (initCell x) range)
        |> List.map2
            (\die ( key, Cell xy _ s ) ->
                ( key, Cell xy die s )
            )
            dice
        |> Dict.fromList


initCell : Int -> Int -> ( XY, Cell )
initCell x y =
    ( ( x, y ), Cell ( x, y ) Die.defaultDie False )


boardToGrid : Board -> Grid
boardToGrid board =
    board
        |> Dict.values
        |> LEx.groupsOf dimensions
