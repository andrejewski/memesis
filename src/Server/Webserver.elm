module Server.Webserver
    exposing
        ( Request
        , Response
        , decodeRequest
        , encodeResponse
        , request
          -- TEMP (will be port)
        , respond
          -- TEMP (will be port)
        )

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Dict exposing (Dict)


type alias Request =
    { method : String
    , url : String
    , headers : Dict String String
    , body : String
    }


type alias Response =
    { statusCode : Int
    , statusMessage : Maybe String
    , headers : Dict String String
    , body : String
    }


request : Int -> (String -> Encode.Value -> msg) -> Sub msg
request httpPort msg =
    Sub.none


respond : String -> Encode.Value -> Cmd msg
respond ref response =
    Cmd.none



-- port httpRequest : Int -> (Context -> Value -> msg) -> Sub msg
-- port httpRespond : Context -> Value -> Cmd msg


encodeStringDict : Dict String String -> Encode.Value
encodeStringDict dict =
    dict
        |> Dict.toList
        |> List.map
            (\( x, y ) ->
                ( x, Encode.string y )
            )
        |> Encode.object


encodeResponse : Response -> Encode.Value
encodeResponse res =
    Encode.object
        [ ( "statusCode", Encode.int res.statusCode )
        , ( "statusMessage"
          , case res.statusMessage of
                Just message ->
                    Encode.string message

                Nothing ->
                    Encode.null
          )
        , ( "headers", encodeStringDict res.headers )
        , ( "body", Encode.string res.body )
        ]



-- Create a new message type which receive the task result
-- Send a command which will callback with that type


decodeRequest : Decoder Request
decodeRequest =
    Decode.map4 Request
        (Decode.field "method" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "headers" (Decode.dict Decode.string))
        (Decode.field "body" Decode.string)
