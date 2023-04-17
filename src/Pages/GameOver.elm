module Pages.GameOver exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Game.GameState exposing (GameState)
import Game.Submissions as Submissions exposing (Submission(..))
import Message exposing (Msg(..))
import UI.Common exposing (scoreColor)
import UI.Palette exposing (..)


view : GameState -> Element Msg
view gameState =
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
