module UI.BoardView exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Game.Board as Board exposing (Board, Grid)
import Game.Cell exposing (Cell(..))
import Game.Die as Die exposing (Die(..))
import Message exposing (Msg(..))
import UI.Palette exposing (..)


view : Board -> Element Msg
view board =
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
                Cell _ die ->
                    ( []
                    , { attrs = [ Background.color shadowGreen ]
                      , onPress = Just (SelectDie cell)
                      }
                    , die
                    )

                AvailableCell _ die ->
                    ( []
                    , { attrs = [ Background.color shadowGreen ]
                      , onPress = Just <| SelectDie cell
                      }
                    , die
                    )

                UnreachableCell _ die ->
                    ( []
                    , { attrs = [ Background.color shadowGreen ]
                      , onPress = Nothing
                      }
                    , die
                    )

                SelectedCell _ die ->
                    ( []
                    , { attrs = [ Background.color lisbonBrown, Font.color atlantis ]
                      , onPress = Nothing
                      }
                    , die
                    )

                CurrentCell _ die ->
                    ( []
                    , { attrs = [ Background.color <| atlantis, Font.color lisbonBrown ]
                      , onPress = Just UnselectDie
                      }
                    , die
                    )

                RollingDieCell _ die ->
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
