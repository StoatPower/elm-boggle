module Die exposing (..)

import Array exposing (Array)
import Random


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


defaultDie : Die
defaultDie =
    "aaaaaa"
        |> String.split ""
        |> Array.fromList
        |> Die 0


defaultDice : List Die
defaultDice =
    dieConfigs
        |> List.map (Die 0)


getFace : Die -> Maybe String
getFace (Die index config) =
    Array.get index config
