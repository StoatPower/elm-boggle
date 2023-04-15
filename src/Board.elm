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


setNewDieFaceForCell : XY -> Int -> Board -> Board
setNewDieFaceForCell key newFace board =
    board
        |> Dict.update key (Maybe.map <| finishRollingDie newFace)


stageShuffle : Board -> Board
stageShuffle board =
    board
        |> Dict.map
            (\_ cell ->
                let
                    ( xy, die ) =
                        cellData cell
                in
                RollingDieCell xy die
            )


isShuffling : Board -> Bool
isShuffling board =
    board
        |> Dict.values
        |> List.any cellIsRollingDie


cellData : Cell -> ( XY, Die )
cellData cell =
    case cell of
        Cell xy die ->
            ( xy, die )

        SelectedCell xy die ->
            ( xy, die )

        CurrentCell xy die ->
            ( xy, die )

        RollingDieCell xy die ->
            ( xy, die )


cellKey : Cell -> XY
cellKey cell =
    cell
        |> cellData
        |> Tuple.first


cellDie : Cell -> Die
cellDie cell =
    cell
        |> cellData
        |> Tuple.second


cellFace : Cell -> Maybe String
cellFace cell =
    cell
        |> cellDie
        |> Die.getFace


cellIsRollingDie : Cell -> Bool
cellIsRollingDie cell =
    case cell of
        RollingDieCell _ _ ->
            True

        _ ->
            False


finishRollingDie : Int -> Cell -> Cell
finishRollingDie newFace cell =
    case cell of
        RollingDieCell xy die ->
            Cell xy <| Die.setFace newFace die

        _ ->
            cell


makeNewSelection : XY -> Maybe XY -> Board -> Board
makeNewSelection newKey maybeOldKey board =
    case maybeOldKey of
        Just oldKey ->
            board
                |> Dict.update newKey (Maybe.map markCurrentCell)
                |> Dict.update oldKey (Maybe.map markSelectedCell)

        Nothing ->
            board
                |> Dict.update newKey (Maybe.map markCurrentCell)


clearSelectedCells : Board -> Board
clearSelectedCells board =
    board
        |> Dict.map (\_ cell -> markCell cell)


markCurrentCell : Cell -> Cell
markCurrentCell cell =
    case cell of
        Cell xy die ->
            CurrentCell xy die

        _ ->
            cell


markSelectedCell : Cell -> Cell
markSelectedCell cell =
    case cell of
        CurrentCell xy die ->
            SelectedCell xy die

        _ ->
            cell


markCell : Cell -> Cell
markCell cell =
    let
        ( xy, die ) =
            cellData cell
    in
    Cell xy die
