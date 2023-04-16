module Submissions exposing (..)

import Scorebook exposing (Score, ScoreResult, Scorebook, Word)


type Submission
    = ValidWord Word Score
    | InvalidWord Word Score


type alias Submissions =
    List Submission


submitWord : Word -> Scorebook -> Submissions -> Submissions
submitWord word scorebook submissions =
    let
        submission =
            scorebook
                |> Scorebook.scoreWord word
                |> toSubmission word
    in
    submission :: submissions


toSubmission : Word -> ScoreResult -> Submission
toSubmission word scoreResult =
    case scoreResult of
        Ok score ->
            ValidWord word score

        Err score ->
            InvalidWord word score
