module Cell exposing (..)

import Die exposing (Die(..))


type alias XY =
    ( Int, Int )


initPosition : Int -> Int -> XY
initPosition x y =
    ( x, y )


type Cell
    = Cell XY Die
    | SelectedCell XY Die
    | CurrentCell XY Die
    | RollingDieCell XY Die


init : Die -> XY -> ( XY, Cell )
init die position =
    ( position, Cell position die )


getData : Cell -> ( XY, Die )
getData cell =
    case cell of
        Cell xy die ->
            ( xy, die )

        SelectedCell xy die ->
            ( xy, die )

        CurrentCell xy die ->
            ( xy, die )

        RollingDieCell xy die ->
            ( xy, die )


getKey : Cell -> XY
getKey cell =
    cell
        |> getData
        |> Tuple.first


getDie : Cell -> Die
getDie cell =
    cell
        |> getData
        |> Tuple.second


getDieFace : Cell -> Maybe String
getDieFace cell =
    cell
        |> getDie
        |> Die.getFace


isRollingDie : Cell -> Bool
isRollingDie cell =
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


markCurrent : Cell -> Cell
markCurrent cell =
    case cell of
        Cell xy die ->
            CurrentCell xy die

        _ ->
            cell


markSelected : Cell -> Cell
markSelected cell =
    case cell of
        CurrentCell xy die ->
            SelectedCell xy die

        _ ->
            cell


markCell : Cell -> Cell
markCell cell =
    let
        ( xy, die ) =
            getData cell
    in
    Cell xy die
