module Pages.HighScores exposing (..)

import Cell exposing (Cell(..))
import Die exposing (Die(..))
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Game exposing (Game(..))
import GameState exposing (Player, Rounds)
import Message exposing (Msg(..))
import RemoteData exposing (RemoteData(..))
import Scorebook exposing (Score)
import Submissions exposing (Submission(..))
import UI.Common exposing (padNumber, scoreColor)
import UI.Palette exposing (..)


view : Rounds -> Element Msg
view rounds =
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
        , UI.Common.standardButtonView
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
