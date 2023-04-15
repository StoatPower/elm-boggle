module Words exposing (..)

import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode
import Http
import RemoteData exposing (WebData)


wordsUrl : String
wordsUrl =
    "https://raw.githubusercontent.com/raun/Scrabble/a9873a163e3ab5d25a209595f738280b144969ce/words.txt"


type alias Words =
    List String


decodeBytes : Decode.Decoder String
decodeBytes =
    Decode.unsignedInt32 BE
        |> Decode.andThen Decode.string


parseWordsText : String -> List String
parseWordsText text =
    String.split "\n" text


getWords : (WebData String -> msg) -> Cmd msg
getWords message =
    Http.get
        { url = wordsUrl
        , expect = Http.expectString (RemoteData.fromResult >> message)
        }
