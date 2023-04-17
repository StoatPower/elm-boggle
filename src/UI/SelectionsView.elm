module UI.SelectionsView exposing (..)

import Cell exposing (Cell(..))
import Die exposing (Die(..))
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Game exposing (Game(..))
import GameState exposing (Selections)
import Message exposing (Msg(..))
import RemoteData exposing (RemoteData(..))
import Scorebook
import Submissions exposing (Submission(..))
import UI.Palette exposing (..)


view : Selections -> Element Msg
view selections =
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