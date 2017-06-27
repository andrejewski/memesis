module Main exposing (..)

import Html exposing (..)
import Level.End as End
import Level.Start as Start
import Level.SuchDoge as SuchDoge


--import Html.Attributes exposing (..)
--import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { level : Level
    }


type Level
    = Start Start.Model
    | End End.Model
    | SuchDoge SuchDoge.Model



-- | GrumpyCat
-- | ForeverAlone
-- | YoDawgIHeard
-- | ScumbagSteve
-- | TheEnd


initialModel : Model
initialModel =
    { level = Start Start.init
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = AdvanceLevel
    | StartMsg Start.Msg
    | SuchDogeMsg SuchDoge.Msg
    | EndMsg End.Msg


advanceLevel : Model -> Model
advanceLevel model =
    case model.level of
        Start _ ->
            { model | level = SuchDoge SuchDoge.init }

        SuchDoge _ ->
            { model | level = End End.init }

        End _ ->
            { model | level = Start Start.init }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toLevel toModel toMsg subAdvance subUpdate subMsg subModel =
            if subAdvance subModel then
                update AdvanceLevel model
            else
                let
                    ( newModel, newCmd ) =
                        subUpdate subMsg subModel
                in
                    ( { model | level = (toModel newModel) }, Cmd.map toMsg newCmd )
    in
        case ( msg, model.level ) of
            ( AdvanceLevel, _ ) ->
                ( (advanceLevel model), Cmd.none )

            ( EndMsg msg, End submodel ) ->
                toLevel End EndMsg End.shouldAdvance End.update msg submodel

            ( StartMsg msg, Start submodel ) ->
                toLevel Start StartMsg Start.shouldAdvance Start.update msg submodel

            ( SuchDogeMsg msg, SuchDoge submodel ) ->
                toLevel SuchDoge SuchDogeMsg SuchDoge.shouldAdvance SuchDoge.update msg submodel

            ( _, _ ) ->
                ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.level of
        Start _ ->
            Sub.map StartMsg Start.subscriptions

        SuchDoge model ->
            Sub.map SuchDogeMsg (SuchDoge.subscriptions model)

        End _ ->
            Sub.map EndMsg End.subscriptions



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ levelView model.level
        ]


levelView : Level -> Html Msg
levelView level =
    case level of
        Start model ->
            Html.map StartMsg (Start.view model)

        End model ->
            Html.map EndMsg (End.view model)

        SuchDoge model ->
            Html.map SuchDogeMsg (SuchDoge.view model)
