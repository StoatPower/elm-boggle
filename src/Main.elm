module Main exposing (..)

import Browser exposing (Document)
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { message : String
    }


defaultModel : Model
defaultModel =
    Model "Hello Simulation"


init : ( Model, Cmd Msg )
init =
    ( defaultModel
    , Cmd.none
    )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "Enter the Matrix"
    , body =
        [ El.layout [] <| text model.message ]
    }
