module UI.SubmissionsView exposing (..)

import Cell exposing (Cell(..))
import Die exposing (Die(..))
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Game exposing (Game(..))
import Message exposing (Msg(..))
import RemoteData exposing (RemoteData(..))
import Scorebook
import Submissions exposing (Submission(..), Submissions)
import UI.Common exposing (scoreColor)
import UI.Palette exposing (..)


view : Submissions -> Element Msg
view submissions =
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
