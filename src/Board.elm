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
    let
        selectedKey =
            Cell.getKey selected
    in
    case maybeLastSelected of
        Just lastSelected ->
            let
                lastSelectedKey =
                    Cell.getKey lastSelected
            in
            -- n-th selection, no cells are pristine
            -- first reset old currently available cells, minus the new key
            -- then revert old current back to selected
            -- then set new key to current
            -- then set our available cells
            board
                |> updateAsUnreachable
                    (Cell.adjacentPositions lastSelectedKey
                        |> List.filter (\xy -> xy /= selectedKey)
                    )
                |> updateAsSelected lastSelectedKey
                |> updateAsCurrent selectedKey
                |> updateCurrentlyAvailableCells selectedKey

        Nothing ->
            -- our first selection, start by setting as current
            -- then marking everything else unreachable
            -- then set our available cells
            board
                |> updateAsCurrent selectedKey
                |> updateAsUnreachable
                    (Dict.keys board
                        |> List.filter (\xy -> xy /= selectedKey)
                    )
                |> updateCurrentlyAvailableCells selectedKey


undoLastSelection : Cell -> Maybe Cell -> Board -> Board
undoLastSelection selected maybePrevSelected board =
    case maybePrevSelected of
        Just prevSelected ->
            let
                selectedKey =
                    Cell.getKey selected

                prevSelectedKey =
                    Cell.getKey prevSelected
            in
            -- first reset old currently available cells, including current cell, to unreachable
            -- then update the previous selected key to current
            -- then update its currently available cells
            board
                |> updateAsUnreachable (selectedKey :: Cell.adjacentPositions selectedKey)
                |> updateAsCurrent prevSelectedKey
                |> updateCurrentlyAvailableCells prevSelectedKey

        Nothing ->
            -- only selection left, so mark all cells as fresh
            board
                |> resetAllCells


updateAsCurrent : XY -> Board -> Board
updateAsCurrent xy board =
    board
        |> updateCellWith Cell.markCurrent xy


updateAsSelected : XY -> Board -> Board
updateAsSelected xy board =
    board
        |> updateCellWith Cell.markSelected xy


updateAsCell : XY -> Board -> Board
updateAsCell xy board =
    board
        |> updateCellWith Cell.markCell xy


updateAsUnreachable : List XY -> Board -> Board
updateAsUnreachable positions board =
    positions
        |> List.foldl (updateCellWith Cell.markUnreachable)
            board


updateCellWith : (Cell -> Cell) -> XY -> Board -> Board
updateCellWith cellMapper xy board =
    Dict.update xy (Maybe.map cellMapper) board


resetAllCells : Board -> Board
resetAllCells board =
    board
        |> Dict.map (\_ cell -> Cell.markCell cell)


updateCurrentlyAvailableCells : XY -> Board -> Board
updateCurrentlyAvailableCells xy board =
    xy
        |> Cell.adjacentPositions
        |> List.filterMap
            (\pos ->
                board
                    |> Dict.get pos
                    |> Maybe.andThen validNextCellSelection
            )
        |> List.foldl (Cell.getKey >> updateCellWith Cell.markAvailable)
            board


validNextCellSelection : Cell -> Maybe Cell
validNextCellSelection cell =
    case cell of
        Cell _ _ ->
            Just cell

        AvailableCell _ _ ->
            Just cell

        UnreachableCell _ _ ->
            Just cell

        _ ->
            Nothing
