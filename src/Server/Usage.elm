module Usage exposing (..)

import Server.Webserver exposing (Request)


main : Program Never Model Msg
main =
    Platform.program
        { init = ( { requests = [] }, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { requests : List Request }


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
