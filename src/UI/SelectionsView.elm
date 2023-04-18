module UI.SelectionsView exposing (..)

import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Game.Cell as Cell
import Game.Die as Die
import Game.GameState as GameState exposing (Selections)
import Game.Scorebook as Scorebook
import Message exposing (Msg(..))
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
            if canSubmitSelections selections then
                ( [ Font.color atlantis ], Just SubmitWord )

            else
                ( [ Font.color chiffon ], Nothing )
    in
    Input.button
        ([ width <| px 65 ] ++ attrs)
        { onPress = msg
        , label = el [ centerX, centerY, Font.letterSpacing 1.5 ] <| text "Submit"
        }


canSubmitSelections : Selections -> Bool
canSubmitSelections selections =
    sumSelections selections
        >= Scorebook.minWordLength


sumSelections : Selections -> Int
sumSelections selections =
    selections
        |> List.filterMap (Cell.getDie >> Die.getFaceValue)
        |> List.sum
