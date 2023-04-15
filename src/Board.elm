module Board exposing (..)

import Dict exposing (Dict)
import Die exposing (Die(..))
import List.Extra as LEx


type alias XY =
    ( Int, Int )


type Cell
    = Cell XY Die
    | SelectedCell XY Die
    | CurrentCell XY Die
    | RollingDieCell XY Die


type alias Board =
    Dict XY Cell


type alias Grid =
    List (List Cell)


dimensions : Int
dimensions =
    5


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
            (\x -> List.map (initPosition x) range)
        |> List.map2 initCell dice
        |> Dict.fromList


initPosition : Int -> Int -> XY
initPosition x y =
    ( x, y )


initCell : Die -> XY -> ( XY, Cell )
initCell die position =
    ( position, Cell position die )


boardToGrid : Board -> Grid
boardToGrid board =
    board
        |> Dict.values
        |> LEx.groupsOf dimensions
