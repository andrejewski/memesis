module Server.Database.Encode exposing (..)

import Json.Encode as Encode exposing (Value)
import Server.Database
    exposing
        ( Model
        , Action(..)
        , Constraint
        , Selector
        , Mutation
        , Operation
        , Connection
        )


connection : Connection -> Value
connection conn =
    Encode.object
        [ ( "uri", Encode.string conn.uri ) ]


model : Model a -> Value
model mod =
    Encode.object
        [ ( "name", Encode.string mod.name )
        , ( "connection", connection mod.connection )
        ]


constraint : Constraint -> Value
constraint con =
    Encode.null


mutation : Mutation -> Value
mutation sel =
    Encode.null


selector : Selector -> Value
selector sel =
    Encode.null


action : Action -> Value
action act =
    case act of
        Select con sel ->
            Encode.object
                [ ( "type", Encode.string "select" )
                , ( "constraint", constraint con )
                , ( "selector", selector sel )
                ]

        Count con ->
            Encode.object
                [ ( "type", Encode.string "count" )
                , ( "constraint", constraint con )
                ]

        Create val ->
            Encode.object
                [ ( "type", Encode.string "create" )
                , ( "value", val )
                ]

        Update con mut ->
            Encode.object
                [ ( "type", Encode.string "update" )
                , ( "constraint", constraint con )
                , ( "mutation", mutation mut )
                ]

        Delete con ->
            Encode.object
                [ ( "type", Encode.string "delete" )
                , ( "constraint", constraint con )
                ]


transaction : Model a -> Action -> Value
transaction mod act =
    Encode.object
        [ ( "model", model mod )
        , ( "action", action act )
        ]
