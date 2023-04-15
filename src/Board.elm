module Board exposing (..)

import Cell exposing (Cell(..), XY)
import Dict exposing (Dict)
import Die exposing (Die(..))
import List.Extra as LEx


type alias Board =
    Dict XY Cell


type alias Grid =
    List (List Cell)


dimensions : Int
dimensions =
    5


init : Board
init =
    let
        range =
            List.range 0 (dimensions - 1)

        dice =
            Die.defaultDice
    in
    range
        |> List.concatMap
            (\x -> List.map (Cell.initPosition x) range)
        |> List.map2 Cell.init dice
        |> Dict.fromList


toGrid : Board -> Grid
toGrid board =
    board
        |> Dict.values
        |> LEx.groupsOf dimensions


setNewDieFaceForCell : XY -> Int -> Board -> Board
setNewDieFaceForCell key newFace board =
    board
        |> Dict.update key (Maybe.map <| Cell.finishRollingDie newFace)


stageShuffle : Board -> Board
stageShuffle board =
    board
        |> Dict.map
            (\_ cell ->
                let
                    ( xy, die ) =
                        Cell.getData cell
                in
                RollingDieCell xy die
            )


isShuffling : Board -> Bool
isShuffling board =
    board
        |> Dict.values
        |> List.any Cell.isRollingDie


makeNewSelection : Cell -> Maybe Cell -> Board -> Board
makeNewSelection selected maybeLastSelected board =
    case maybeLastSelected of
        Just lastSelected ->
            board
                |> Dict.update (Cell.getKey selected) (Maybe.map Cell.markCurrent)
                |> Dict.update (Cell.getKey lastSelected) (Maybe.map Cell.markSelected)

        Nothing ->
            board
                |> Dict.update (Cell.getKey selected) (Maybe.map Cell.markCurrent)


undoLastSelection : Cell -> Maybe Cell -> Board -> Board
undoLastSelection selected maybePrevSelected board =
    case maybePrevSelected of
        Just prevSelected ->
            board
                |> Dict.update (Cell.getKey selected) (Maybe.map Cell.markCell)
                |> Dict.update (Cell.getKey prevSelected) (Maybe.map Cell.markCurrent)

        Nothing ->
            board
                |> Dict.update (Cell.getKey selected) (Maybe.map Cell.markCell)


clearSelectedCells : Board -> Board
clearSelectedCells board =
    board
        |> Dict.map (\_ cell -> Cell.markCell cell)
