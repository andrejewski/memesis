module Usage exposing (..)

import Dict
import Json.Decode as Decode
import Server.Webserver
    exposing
        ( Request
        , Response
        , request
        , respond
        , decodeRequest
        , encodeResponse
        )


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
    = ReceiveRequest String Request
    | ReceiveJunk String String


plainResponse : Response
plainResponse =
    { url = ""
    , status = 200
    , headers = Dict.empty
    , cookies = Dict.empty
    , body = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveJunk ref error ->
            let
                response =
                    { plainResponse
                        | status = 500
                        , body = error
                    }
            in
                ( model, reply ref response )

        ReceiveRequest ref request ->
            let
                response =
                    { plainResponse
                        | status = 200
                        , url = request.url
                        , body = "Successful request, mate!"
                    }
            in
                ( model, reply ref response )


subscriptions : Model -> Sub Msg
subscriptions model =
    requests 8080



-- PLUMMING


reply : String -> Response -> Cmd Msg
reply ref response =
    respond ref (encodeResponse response)


requests : Int -> Sub Msg
requests serverPort =
    request serverPort
        (\ref request ->
            let
                result =
                    Decode.decodeValue decodeRequest request
            in
                case result of
                    Err error ->
                        ReceiveJunk ref error

                    Ok request ->
                        ReceiveRequest ref request
        )
