module Game.Board exposing (..)

import Dict exposing (Dict)
import Game.Cell as Cell exposing (Cell(..), XY)
import Game.Die as Die exposing (Die(..))
import List.Extra as LEx
import Random


type alias Board =
    Dict XY Cell


type alias Grid =
    List (List Cell)


dimensions : Int
dimensions =
    5


size : Board -> Int
size board =
    Dict.size board


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


swapCellDice : XY -> XY -> Board -> Board
swapCellDice key1 key2 board =
    if key1 /= key2 then
        let
            maybeDie1 =
                getCell key1 board
                    |> Maybe.map Cell.getDie

            maybeDie2 =
                getCell key2 board
                    |> Maybe.map Cell.getDie
        in
        case ( maybeDie1, maybeDie2 ) of
            ( Just die1, Just die2 ) ->
                board
                    |> Dict.update key1 (Maybe.map <| Cell.setDie die2)
                    |> Dict.update key2 (Maybe.map <| Cell.setDie die1)

            ( _, _ ) ->
                board

    else
        board


randomSwap : Random.Generator ( XY, XY )
randomSwap =
    Random.pair randomXY randomXY


randomXY : Random.Generator XY
randomXY =
    Random.pair
        (Random.int 0 (dimensions - 1))
        (Random.int 0 (dimensions - 1))


toGrid : Board -> Grid
toGrid board =
    board
        |> Dict.values
        |> LEx.groupsOf dimensions


getCell : XY -> Board -> Maybe Cell
getCell xy board =
    Dict.get xy board


getCells : Board -> List Cell
getCells board =
    Dict.values board


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


diceAreRolling : Board -> Bool
diceAreRolling board =
    board
        |> getCells
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
