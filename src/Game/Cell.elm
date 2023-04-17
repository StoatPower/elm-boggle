module Game.Cell exposing (..)

import Game.Die as Die exposing (Die(..))


type alias XY =
    ( Int, Int )


initPosition : Int -> Int -> XY
initPosition x y =
    ( x, y )


type Cell
    = Cell XY Die
    | AvailableCell XY Die
    | UnreachableCell XY Die
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

        AvailableCell xy die ->
            ( xy, die )

        UnreachableCell xy die ->
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


setDie : Die -> Cell -> Cell
setDie die cell =
    case cell of
        Cell xy _ ->
            Cell xy die

        AvailableCell xy _ ->
            AvailableCell xy die

        UnreachableCell xy _ ->
            UnreachableCell xy die

        SelectedCell xy _ ->
            SelectedCell xy die

        CurrentCell xy _ ->
            CurrentCell xy die

        RollingDieCell xy _ ->
            RollingDieCell xy die


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

        AvailableCell xy die ->
            CurrentCell xy die

        SelectedCell xy die ->
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


markAvailable : Cell -> Cell
markAvailable cell =
    case cell of
        Cell xy die ->
            AvailableCell xy die

        UnreachableCell xy die ->
            AvailableCell xy die

        _ ->
            cell


markUnreachable : Cell -> Cell
markUnreachable cell =
    case cell of
        Cell xy die ->
            UnreachableCell xy die

        AvailableCell xy die ->
            UnreachableCell xy die

        CurrentCell xy die ->
            UnreachableCell xy die

        _ ->
            cell


adjacentPositions : XY -> List XY
adjacentPositions xy =
    [ westPosition
    , northWestPosition
    , northPosition
    , northEastPosition
    , eastPosition
    , southEastPosition
    , southPosition
    , southWestPosition
    ]
        |> List.filterMap (\fn -> fn xy)


westPosition : XY -> Maybe XY
westPosition xy =
    case xy of
        ( 0, _ ) ->
            Nothing

        ( x, y ) ->
            Just ( x - 1, y )


northWestPosition : XY -> Maybe XY
northWestPosition xy =
    case xy of
        ( 0, _ ) ->
            Nothing

        ( _, 0 ) ->
            Nothing

        ( x, y ) ->
            Just ( x - 1, y - 1 )


northPosition : XY -> Maybe XY
northPosition xy =
    case xy of
        ( _, 0 ) ->
            Nothing

        ( x, y ) ->
            Just ( x, y - 1 )


northEastPosition : XY -> Maybe XY
northEastPosition xy =
    case xy of
        ( 4, _ ) ->
            Nothing

        ( _, 0 ) ->
            Nothing

        ( x, y ) ->
            Just ( x + 1, y - 1 )


eastPosition : XY -> Maybe XY
eastPosition xy =
    case xy of
        ( 4, _ ) ->
            Nothing

        ( x, y ) ->
            Just ( x + 1, y )


southEastPosition : XY -> Maybe XY
southEastPosition xy =
    case xy of
        ( 4, _ ) ->
            Nothing

        ( _, 4 ) ->
            Nothing

        ( x, y ) ->
            Just ( x + 1, y + 1 )


southPosition : XY -> Maybe XY
southPosition xy =
    case xy of
        ( _, 4 ) ->
            Nothing

        ( x, y ) ->
            Just ( x, y + 1 )


southWestPosition : XY -> Maybe XY
southWestPosition xy =
    case xy of
        ( 0, _ ) ->
            Nothing

        ( _, 4 ) ->
            Nothing

        ( x, y ) ->
            Just ( x - 1, y + 1 )
