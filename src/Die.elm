module Die exposing (..)

import Array exposing (Array)
import List exposing (indexedMap, length)
import List.Extra exposing (getAt)
import Random exposing (Generator, andThen, map, step)
import Set exposing (Set, empty, insert)


sides : Int
sides =
    6


type Die
    = Die Int DieConfig


type alias DieConfig =
    Array String


dieConfigs : List DieConfig
dieConfigs =
    [ "aaafrs"
    , "aaeeee"
    , "aafirs"
    , "adennn"
    , "aeeeem"
    , "aeegmu"
    , "aegmnn"
    , "afirsy"
    , "bjkqxz"
    , "ccenst"
    , "ceiilt"
    , "ceilpt"
    , "ceipst"
    , "ddhnot"
    , "dhhlor"
    , "dhlnor"
    , "dhlnor"
    , "eiiitt"
    , "emottt"
    , "ensssu"
    , "fiprsy"
    , "gorrvw"
    , "iprrry"
    , "nootuw"
    , "ooottu"
    ]
        |> List.map (String.split "")
        |> List.map Array.fromList


setFace : Int -> Die -> Die
setFace newFace (Die _ config) =
    Die newFace config


roll : Random.Generator Int
roll =
    Random.int 0 (sides - 1)


defaultDice : List Die
defaultDice =
    dieConfigs
        |> List.map (Die 0)


getFace : Die -> Maybe String
getFace (Die index config) =
    Array.get index config
        |> Maybe.map augmentQ


augmentQ : String -> String
augmentQ char =
    if char == "q" then
        "qu"

    else
        char
