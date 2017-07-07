module Server.Webserver
    exposing
        ( Context
        , Request
        , Response
        , decodeRequest
        , encodeResponse
        )

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Dict exposing (Dict)


type alias Context =
    String


type alias Request =
    { method : String
    , url : String
    , headers : Dict String String
    , cookies : Dict String String
    }


type alias Response =
    { url : String
    , status : Int
    , headers : Dict String String
    , cookies : Dict String String
    , body : String
    }



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
        [ ( "url", Encode.string res.url )
        , ( "status", Encode.int res.status )
        , ( "headers", encodeStringDict res.headers )
        , ( "cookies", encodeStringDict res.cookies )
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
        (Decode.field "cookies" (Decode.dict Decode.string))
