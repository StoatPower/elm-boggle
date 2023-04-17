module Game.GameState exposing (..)

import Game.Board as Board exposing (Board)
import Game.Cell as Cell exposing (Cell(..), XY)
import Game.Die exposing (DieConfigIdx)
import Game.Scorebook exposing (Score, Scorebook, Word)
import Game.Submissions as Submissions exposing (Submission(..), Submissions)


type alias Player =
    String


type alias Rounds =
    List ( Player, Score )


type alias Selections =
    List Cell


type alias GameState =
    { board : Board
    , selections : Selections
    , submissions : Submissions
    , player : Maybe Player
    , rounds : Rounds
    , scorebook : Scorebook
    , remainingSeconds : Int
    , remainingShuffleMillis : Int
    }


type alias KeepShuffling =
    Bool


init : Scorebook -> GameState
init scorebook =
    { board = Board.init |> Board.stageShuffle
    , selections = []
    , submissions = []
    , player = Nothing
    , rounds = []
    , scorebook = scorebook
    , remainingSeconds = defaultTimeSeconds
    , remainingShuffleMillis = shuffleForMillis
    }


reInit : GameState -> GameState
reInit gameState =
    { gameState
        | board = gameState.board |> Board.stageShuffle
        , selections = []
        , submissions = []
        , remainingSeconds = defaultTimeSeconds
        , player = Nothing
    }


newDieFace : XY -> DieConfigIdx -> GameState -> GameState
newDieFace key newFace gameState =
    let
        newBoard =
            gameState.board
                |> Board.setNewDieFaceForCell key newFace
    in
    { gameState | board = newBoard }


swapDice : XY -> XY -> GameState -> GameState
swapDice xy1 xy2 gameState =
    let
        newBoard =
            gameState.board
                |> Board.swapCellDice xy1 xy2
    in
    { gameState | board = newBoard }


selectDie : Cell -> GameState -> GameState
selectDie cell gameState =
    let
        lastSelected =
            List.head gameState.selections

        newBoard =
            gameState.board
                |> Board.makeNewSelection
                    cell
                    lastSelected

        newSelections =
            cell :: gameState.selections
    in
    { gameState | board = newBoard, selections = newSelections }


unselectDie : GameState -> GameState
unselectDie gameState =
    let
        ( newBoard, newSelections ) =
            case gameState.selections of
                [] ->
                    ( gameState.board, [] )

                h :: [] ->
                    ( gameState.board
                        |> Board.undoLastSelection h Nothing
                    , []
                    )

                h :: n :: t ->
                    ( gameState.board
                        |> Board.undoLastSelection h (Just n)
                    , n :: t
                    )
    in
    { gameState | board = newBoard, selections = newSelections }


submitWord : GameState -> GameState
submitWord gameState =
    let
        newBoard =
            Board.resetAllCells gameState.board

        newSubmissions =
            gameState.submissions
                |> Submissions.submitWord
                    (selectionsToWord gameState.selections)
                    gameState.scorebook
    in
    { gameState
        | board = newBoard
        , selections = []
        , submissions = newSubmissions
    }


updatePlayer : String -> GameState -> GameState
updatePlayer name gameState =
    { gameState | player = Just name }


submitPlayerScore : Player -> Score -> GameState -> GameState
submitPlayerScore player finalScore gameState =
    let
        newRounds =
            ( player, finalScore ) :: gameState.rounds
    in
    { gameState
        | rounds = newRounds
        , remainingShuffleMillis = shuffleForMillis
    }


countdownShuffleTimer : GameState -> Maybe ( GameState, KeepShuffling )
countdownShuffleTimer gameState =
    case ( gameState.remainingShuffleMillis > 0, Board.diceAreRolling gameState.board ) of
        ( True, True ) ->
            Just
                ( { gameState | remainingShuffleMillis = gameState.remainingShuffleMillis - 1 }
                , False
                )

        ( True, False ) ->
            Just
                ( { gameState | remainingShuffleMillis = gameState.remainingShuffleMillis - 1 }
                , True
                )

        ( _, _ ) ->
            Nothing


countdownGameTimer : GameState -> Maybe GameState
countdownGameTimer gameState =
    if gameState.remainingSeconds > 0 then
        Just <| { gameState | remainingSeconds = gameState.remainingSeconds - 1 }

    else
        Nothing


selectionsToWord : Selections -> Word
selectionsToWord selections =
    selections
        |> List.reverse
        |> List.filterMap Cell.getDieFace
        |> String.join ""


shuffleForMillis : Int
shuffleForMillis =
    100


defaultTimeSeconds : Int
defaultTimeSeconds =
    180
