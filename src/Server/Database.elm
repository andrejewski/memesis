module Server.Database exposing (..)

import Task exposing (Task)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)
import Json.Decode exposing (Decoder)


type alias Tag =
    String



-- port dbResponse : (Tag -> Value -> msg) -> Sub msg
-- port dbRequest : Connection -> Tag -> Value -> Cmd msg


type Action
    = Select Constraint Selector
    | Count Constraint
    | Create Value
    | Update Constraint Mutation
    | Delete Constraint


type alias Field =
    String


type Constraint
    = FieldConstraint (Dict Field Constraint)
    | QueryConstraint Operation
    | LogicConstraint


type Operation
    = LessThan Value
    | GreaterThan Value
    | EqualTo Value


type LogicConstraint
    = And (List Constraint)
    | Or (List Constraint)
    | Not Constraint


type alias Selector =
    { count : Int
    , offset : Int
    , select : Maybe (List Field)
    , sort : List ( Field, Sort )
    }


type Sort
    = Ascending
    | Descending


type alias Mutation =
    Dict Field FieldAction


type FieldAction
    = SetField Value -- TODO: define atomic updates


type Error
    = BadConnection
    | BadModel
    | MissingPrimaryKey


findWhere : Field -> Value -> Selector -> Action
findWhere field value selector =
    let
        constraint =
            FieldConstraint (Dict.singleton field (QueryConstraint (EqualTo value)))
    in
        Select constraint selector


findOneWhere : Model a -> Field -> Value -> Task Error (Maybe a)
findOneWhere model field value =
    let
        selector =
            { count = 1
            , offset = 0
            , select = Nothing
            , sort = []
            }
    in
        findWhere field value selector
            |> toTask model
            |> Task.map List.head


findById : Model a -> Value -> Task Error (Maybe a)
findById model id =
    case model.primaryKey of
        Just key ->
            findOneWhere model key id

        Nothing ->
            Task.fail MissingPrimaryKey


updateById : Model a -> Value -> Mutation -> Task Error Never
updateById model id mutation =
    Task.fail MissingPrimaryKey


type alias Connection =
    { uri : String
    }


connectionJson : Connection -> Value
connectionJson connection =
    Encode.object
        [ ( "uri", Encode.string connection.uri ) ]


type alias Model a =
    { name : String
    , primaryKey : Maybe String
    , connection : Connection
    , decoder : Decoder a
    }


type alias Request a =
    { context : String
    , model : Model a
    , action : Action
    }


toTask : Model a -> Action -> Task Error (List a)
toTask action model =
    Task.succeed []
