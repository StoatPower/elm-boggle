module Scorebook exposing (..)

import Http
import RemoteData exposing (WebData)
import StringTrie as Trie exposing (Trie)


type alias Score =
    Int


type alias Scorebook =
    Trie Score


type alias Word =
    String


type alias ScoreResult =
    Result Score Score


minWordLength : Int
minWordLength =
    3


wordsUrl : String
wordsUrl =
    "https://raw.githubusercontent.com/raun/Scrabble/a9873a163e3ab5d25a209595f738280b144969ce/words.txt"


buildScorebook : String -> Scorebook
buildScorebook text =
    text
        |> String.split "\n"
        |> List.map (String.trim >> String.toLower >> defaultWordScore)
        |> List.foldl
            (\( word, score ) trie ->
                Trie.insert word score trie
            )
            Trie.empty


getScorebookSource : (WebData String -> msg) -> Cmd msg
getScorebookSource message =
    Http.get
        { url = wordsUrl
        , expect = Http.expectString (RemoteData.fromResult >> message)
        }


defaultWordScore : Word -> ( Word, Score )
defaultWordScore word =
    let
        length =
            String.length word

        score =
            if length == 3 || length == 4 then
                1

            else if length == 5 then
                2

            else if length == 6 then
                3

            else if length == 7 then
                5

            else if length >= 8 then
                11

            else
                0
    in
    ( word, score )


invalidWordScore : Score
invalidWordScore =
    -2


scoreWord : Word -> Scorebook -> ScoreResult
scoreWord word trie =
    trie
        |> Trie.get word
        |> Result.fromMaybe invalidWordScore


fmtScore : Score -> String
fmtScore score =
    if score >= 0 then
        "+" ++ String.fromInt score

    else
        String.fromInt score
