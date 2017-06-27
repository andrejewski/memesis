module Level.End exposing (..)

import Html exposing (..)
import AnimationFrame
import Keyboard
import Color
import Time exposing (Time)
import Collage exposing (..)
import Text exposing (Text)
import Element


type alias Model =
    { x : Float
    , y : Float
    , direction : Maybe Float
    }


type Msg
    = Frame Time
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode


init : Model
init =
    { x = -170
    , y = 180
    , direction = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame time ->
            let
                newModel =
                    model
                        |> gravity time
                        |> movement time
            in
                ( newModel, Cmd.none )

        KeyDown key ->
            ( (updateDirection key model), Cmd.none )

        KeyUp _ ->
            ( { model | direction = Nothing }, Cmd.none )


updateDirection : Keyboard.KeyCode -> Model -> Model
updateDirection key model =
    case key of
        37 ->
            { model | direction = Just -1 }

        39 ->
            { model | direction = Just 1 }

        _ ->
            model


gravity : Time -> Model -> Model
gravity time model =
    let
        minHeight =
            -176
    in
        { model | y = max minHeight (model.y - (0.25 * time)) }


movement : Time -> Model -> Model
movement time model =
    case model.direction of
        Just direction ->
            let
                newX =
                    model.x
                        + (0.25 * direction * time)
                        |> max -175
                        |> min 175
            in
                { model | x = newX }

        Nothing ->
            model


isWithin : Float -> Float -> Float -> Bool
isWithin min max num =
    num > min && num < max


shouldAdvance : Model -> Bool
shouldAdvance model =
    isWithin 150 170 model.x


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , AnimationFrame.diffs Frame
        ]


background : Form
background =
    Collage.rect 400 400
        |> Collage.filled Color.black


width : Float
width =
    400


height : Float
height =
    400


floor : Form
floor =
    rect width 5
        |> filled Color.gray


block : Form
block =
    rect 20 20
        |> filled Color.gray


title : Form
title =
    "You won, yay.."
        |> Text.fromString
        |> Text.monospace
        |> Text.color Color.green
        |> Text.height 40
        |> Element.centered
        |> Collage.toForm


subtitle : Form
subtitle =
    "By Chris Andrejewski"
        |> Text.fromString
        |> Text.monospace
        |> Text.color Color.blue
        |> Text.height 20
        |> Element.centered
        |> Collage.toForm


character : Form
character =
    Collage.group
        [ characterHead
        , characterBody
        ]


characterHead : Form
characterHead =
    circle 5
        |> filled Color.white
        |> moveY 6


characterBody : Form
characterBody =
    rect 10 15
        |> filled Color.white
        |> moveY -10


door : Form
door =
    rect 20 40
        |> filled Color.purple


help : Bool -> Form
help isGood =
    let
        text =
            if isGood then
                "Good job, child!"
            else
                "Restart here =>"
    in
        text
            |> Text.fromString
            |> Text.monospace
            |> Text.color Color.brown
            |> Element.centered
            |> toForm


view : Model -> Html Msg
view model =
    collage
        400
        400
        [ background
        , title
            |> Collage.moveY 20
        , subtitle
            |> Collage.moveY -20
        , floor
            |> moveY -195
        , block
            |> move ( -190, -185 )
        , block
            |> move ( 190, -185 )
        , door
            |> move ( 160, -172 )
        , help False
            |> move ( 80, -160 )
        , character
            |> move ( model.x, model.y )
        ]
        |> Element.toHtml
