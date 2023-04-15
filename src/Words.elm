module Words exposing (..)

import Http
import RemoteData exposing (WebData)


wordsUrl : String
wordsUrl =
    "https://api.github.com/repos/raun/Scrabble/contents/words.txt"


type alias Words =
    List String


parseWordsText : String -> List String
parseWordsText text =
    String.split "\n" text


getWords : (WebData String -> msg) -> Cmd msg
getWords message =
    Http.get
        { url = wordsUrl
        , expect = Http.expectString (RemoteData.fromResult >> message)
        }
