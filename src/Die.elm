module Die exposing (..)

import Array exposing (Array)
import List
import Random


sides : Int
sides =
    6


type alias DieConfigIdx =
    Int


type alias DieId =
    Int


type alias DieConfig =
    Array Char


type Die
    = Die DieId DieConfigIdx DieConfig


dieIdsAndConfigs : List ( DieId, DieConfig )
dieIdsAndConfigs =
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
        |> List.foldl
            (\cfg ( id, cfgs ) ->
                -- 1. increment our index for next config's id
                -- 2. pair our current id with initialized die config and push to list
                ( id + 1, ( id, initDieConfig cfg ) :: cfgs )
            )
            ( 1, [] )
        |> Tuple.second


initDieConfig : String -> Array Char
initDieConfig unprocessedConfig =
    unprocessedConfig
        |> String.toList
        |> Array.fromList


setFace : Int -> Die -> Die
setFace newFace (Die id _ config) =
    Die id newFace config


roll : Random.Generator Int
roll =
    Random.int 0 (sides - 1)


defaultDice : List Die
defaultDice =
    dieIdsAndConfigs
        |> List.map (\( id, cfg ) -> Die id 0 cfg)


getFace : Die -> Maybe String
getFace (Die _ index config) =
    Array.get index config
        |> Maybe.map String.fromChar
        |> Maybe.map augmentQ


augmentQ : String -> String
augmentQ char =
    if char == "q" then
        "qu"

    else
        char
