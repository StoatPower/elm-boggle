module UI.TimerView exposing (..)

import Element exposing (..)
import Element.Font as Font
import Message exposing (Msg(..))
import UI.Common exposing (padNumber)
import UI.Palette exposing (..)


view : Int -> Element Msg
view elapsedSeconds =
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
