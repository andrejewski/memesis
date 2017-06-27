module Level.SuchDoge exposing (..)

import Html exposing (..)
import Time exposing (..)
import Color
import Array exposing (Array)
import Collage exposing (..)
import Element exposing (image)
import Text exposing (..)


type alias Model =
    { isSpeaking : Bool
    , sentences : Array String
    , currentSentence : Int
    , currentCharacter : Int
    }


type Msg
    = Speak Time


init : Model
init =
    { isSpeaking = True
    , sentences = intro
    , currentSentence = 0
    , currentCharacter = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Speak _ ->
            case Array.get model.currentSentence model.sentences of
                Just sentence ->
                    if model.currentCharacter == String.length sentence then
                        case Array.get model.currentSentence model.sentences of
                            Just nextSentence ->
                                ( { model
                                    | currentSentence = model.currentSentence + 1
                                    , currentCharacter = 0
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                ( { model | isSpeaking = False }, Cmd.none )
                    else
                        ( { model | currentCharacter = model.currentCharacter + 1 }, Cmd.none )

                Nothing ->
                    ( { model | isSpeaking = False }, Cmd.none )


dogeFace : Form
dogeFace =
    image 50 50 "doge.gif"
        |> toForm


intro : Array String
intro =
    Array.fromList
        [ "Welcome to the Doge Park.  "
        , "You must entertain me.  "
        , "Much fleeing, such time.  "
        , "WOW, LET'S GO !!!  "
        ]


dialogueSnippet : Int -> String -> String
dialogueSnippet end sentence =
    String.slice 0 end sentence


gameView : Model -> List Form
gameView model =
    case Array.get model.currentSentence model.sentences of
        Just sentence ->
            [ dialogueSnippet model.currentCharacter sentence
                |> fromString
                |> color Color.white
                |> monospace
                |> Element.leftAligned
                |> toForm
                |> moveY 150
            , dogeFace
                |> move ( -150, 150 )
            ]

        Nothing ->
            [ dogeFace
                |> move ( -150, 150 )
            ]


shouldAdvance : Model -> Bool
shouldAdvance model =
    not model.isSpeaking


view : Model -> Html Msg
view model =
    collage
        400
        400
        (gameView model)
        |> Element.toHtml


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isSpeaking then
        Time.every (150 * Time.millisecond) Speak
    else
        Time.every Time.millisecond Speak
