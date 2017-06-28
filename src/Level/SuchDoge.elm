module Level.SuchDoge exposing (..)

import Html exposing (..)
import Time exposing (..)
import Color exposing (Color)
import AnimationFrame
import Keyboard
import Random exposing (Generator)
import Array exposing (Array)
import Collage exposing (..)
import Element exposing (image)
import Text exposing (..)


type alias Model =
    { step : Step
    , currentSentence : Int
    , currentCharacter : Int
    , player : Point
    , directionX : Direction
    , directionY : Direction
    , roundTicks : Int
    }


type Direction
    = Positive
    | Zero
    | Negative


type Msg
    = Speak Time
    | StepForward
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | Frame Time
    | MoveEnemy Time Int ( Direction, Direction )
    | RoundTick Time


init : Model
init =
    { step = SpeechStep Intro
    , currentSentence = 0
    , currentCharacter = 0
    , player = ( 100, -100 )
    , directionX = Zero
    , directionY = Zero
    , roundTicks = 0
    }


type alias Point =
    ( Float, Float )


type alias Enemy =
    { id : Int
    , text : String
    , color : Color
    , position : Point
    }


type alias RoundModel =
    { speed : Float
    , enemies : List Enemy
    }


enemyStart : Point
enemyStart =
    ( -140, 140 )


initEasy : RoundModel
initEasy =
    { speed = 0.8
    , enemies =
        [ { id = 1
          , text = "WOW"
          , color = Color.blue
          , position = enemyStart
          }
        ]
    }


initMedium : RoundModel
initMedium =
    { speed = 0.9
    , enemies =
        [ { id = 1
          , text = "SUCH"
          , color = Color.green
          , position = enemyStart
          }
        , { id = 2
          , text = "WOW"
          , color = Color.red
          , position = enemyStart
          }
        ]
    }


initHard : RoundModel
initHard =
    { speed = 1
    , enemies =
        [ { id = 1
          , text = "MUCH"
          , color = Color.green
          , position = enemyStart
          }
        , { id = 2
          , text = "WOW"
          , color = Color.red
          , position = enemyStart
          }
        ]
    }


initFinal : RoundModel
initFinal =
    { speed = 1.1
    , enemies =
        [ { id = 1
          , text = "MUCH"
          , color = Color.yellow
          , position = enemyStart
          }
        , { id = 2
          , text = "DOGE"
          , color = Color.brown
          , position = enemyStart
          }
        , { id = 3
          , text = "WOW"
          , color = Color.orange
          , position = enemyStart
          }
        ]
    }


type Round
    = Easy RoundModel
    | Medium RoundModel
    | Hard RoundModel
    | Final RoundModel


type Speech
    = Intro
    | PostEasy
    | PostMedium
    | PostHard
    | PostFinal


type Step
    = SpeechStep Speech
    | RoundStep Round
    | Finish


speechText : Speech -> Array String
speechText speech =
    case speech of
        Intro ->
            Array.fromList
                [ "Welcome to the Doge Park.  "
                , "U must entertain me.  "
                , "Much fleeing, such time.  "
                , "WOW, LET'S GO !!!  "
                ]

        PostEasy ->
            Array.fromList
                [ "Such is life. "
                , "Now try twice. "
                ]

        PostMedium ->
            Array.fromList
                [ "Much of life. "
                , "Boost ur strife. "
                ]

        PostHard ->
            Array.fromList
                [ "So good. "
                , "Maybe u go all the way. "
                , "Or not buddy. "
                ]

        PostFinal ->
            Array.fromList
                [ "Ok u pass. "
                , "Thank u 4 entertaining me. "
                ]


updateDirection : Bool -> Keyboard.KeyCode -> Model -> Model
updateDirection isDown key model =
    if isDown then
        case key of
            37 ->
                { model | directionX = Negative }

            38 ->
                { model | directionY = Positive }

            39 ->
                { model | directionX = Positive }

            40 ->
                { model | directionY = Negative }

            _ ->
                model
    else
        case key of
            37 ->
                if model.directionX == Negative then
                    { model | directionX = Zero }
                else
                    model

            38 ->
                if model.directionY == Positive then
                    { model | directionY = Zero }
                else
                    model

            39 ->
                if model.directionX == Positive then
                    { model | directionX = Zero }
                else
                    model

            40 ->
                if model.directionY == Negative then
                    { model | directionY = Zero }
                else
                    model

            _ ->
                model


updateStep : Model -> ( Model, Cmd Msg )
updateStep model =
    let
        toStep step model =
            ( { model | step = step }, Cmd.none )
    in
        case model.step of
            SpeechStep speech ->
                case speech of
                    Intro ->
                        toStep (RoundStep (Easy initEasy)) model

                    PostEasy ->
                        toStep (RoundStep (Medium initMedium)) model

                    PostMedium ->
                        toStep (RoundStep (Hard initHard)) model

                    PostHard ->
                        toStep (RoundStep (Final initFinal)) model

                    PostFinal ->
                        toStep Finish model

            RoundStep round ->
                case round of
                    Easy _ ->
                        toStep (SpeechStep PostEasy) model

                    Medium _ ->
                        toStep (SpeechStep PostMedium) model

                    Hard _ ->
                        toStep (SpeechStep PostHard) model

                    Final _ ->
                        toStep (SpeechStep PostFinal) model

            Finish ->
                ( model, Cmd.none )


directionSign : Direction -> Float
directionSign direction =
    case direction of
        Positive ->
            1

        Negative ->
            -1

        Zero ->
            0


clamp : Float -> Float -> Float -> Float
clamp mini maxi num =
    num
        |> max mini
        |> min maxi


updatePlayer : Time -> Model -> Model
updatePlayer time model =
    let
        signX =
            directionSign model.directionX

        signY =
            directionSign model.directionY

        ( oldX, oldY ) =
            model.player

        newX =
            oldX
                + (0.25 * signX * time)
                |> clamp -190 190

        newY =
            oldY
                + (0.25 * signY * time)
                |> clamp -190 190
    in
        { model | player = ( newX, newY ) }


roundModel : Round -> RoundModel
roundModel round =
    case round of
        Easy r ->
            r

        Medium r ->
            r

        Hard r ->
            r

        Final r ->
            r


mapRound : (RoundModel -> RoundModel) -> Round -> Round
mapRound fn round =
    case round of
        Easy r ->
            Easy (fn r)

        Medium r ->
            Medium (fn r)

        Hard r ->
            Hard (fn r)

        Final r ->
            Final (fn r)


rainbow : Color -> Color
rainbow color =
    if color == Color.green then
        Color.blue
    else if color == Color.blue then
        Color.brown
    else if color == Color.brown then
        Color.yellow
    else if color == Color.yellow then
        Color.red
    else if color == Color.red then
        Color.green
    else
        color


updateEnemy : Time -> Enemy -> Enemy
updateEnemy time enemy =
    { enemy | color = rainbow enemy.color }


updateEnemyDirection : Float -> Time -> ( Direction, Direction ) -> Enemy -> Enemy
updateEnemyDirection speed time ( dx, dy ) enemy =
    let
        signX =
            directionSign dx

        signY =
            directionSign dy

        ( oldX, oldY ) =
            enemy.position

        newX =
            oldX
                + (speed * signX * time)
                |> clamp -190 190

        newY =
            oldY
                + (speed * signY * time)
                |> clamp -190 190
    in
        { enemy | position = ( newX, newY ) }


updateRound : Time -> RoundModel -> RoundModel
updateRound time round =
    { round | enemies = List.map (updateEnemy time) round.enemies }


randomVector : Generator ( Direction, Direction )
randomVector =
    Random.pair randomDirection randomDirection


randomDirection : Generator Direction
randomDirection =
    Random.map
        (\bool ->
            if bool then
                Positive
            else
                Negative
        )
        Random.bool


isWithin : Float -> Float -> Float -> Bool
isWithin min max num =
    num > min && num < max


isLoser : List Enemy -> Point -> Bool
isLoser enemies ( x, y ) =
    List.any
        (\e ->
            let
                ( ex, ey ) =
                    e.position

                widthGap =
                    30

                heightGap =
                    20

                inside ax gap =
                    isWithin (ax - gap) (ax + gap)
            in
                inside ex widthGap x && inside ey heightGap y
        )
        enemies


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.step ) of
        ( StepForward, _ ) ->
            updateStep
                { model
                    | roundTicks = 0
                    , currentSentence = 0
                    , currentCharacter = 0
                }

        ( KeyUp key, _ ) ->
            ( (updateDirection False key model), Cmd.none )

        ( KeyDown key, _ ) ->
            ( (updateDirection True key model), Cmd.none )

        ( RoundTick _, RoundStep _ ) ->
            let
                newTick =
                    model.roundTicks + 1
            in
                if newTick == 30 then
                    update StepForward model
                else
                    ( { model | roundTicks = newTick }, Cmd.none )

        ( Frame time, SpeechStep _ ) ->
            ( (updatePlayer time model), Cmd.none )

        ( Frame time, RoundStep round ) ->
            let
                playerModel =
                    updatePlayer time model

                newRound =
                    mapRound (updateRound time) round

                newModel =
                    { playerModel | step = RoundStep newRound }

                newCmd =
                    round
                        |> roundModel
                        |> .enemies
                        |> List.map
                            (\{ id } ->
                                Random.generate (MoveEnemy time id) randomVector
                            )
                        |> Cmd.batch
            in
                ( newModel, newCmd )

        ( MoveEnemy time id vector, RoundStep round ) ->
            let
                newRound =
                    mapRound
                        (\round ->
                            { round
                                | enemies =
                                    List.map
                                        (\enemy ->
                                            if enemy.id == id then
                                                updateEnemyDirection round.speed time vector enemy
                                            else
                                                enemy
                                        )
                                        round.enemies
                            }
                        )
                        round
            in
                if isLoser (newRound |> roundModel |> .enemies) model.player then
                    ( { init | step = RoundStep (Easy initEasy) }, Cmd.none )
                else
                    ( { model | step = RoundStep newRound }, Cmd.none )

        ( Speak _, SpeechStep speech ) ->
            case Array.get model.currentSentence (speechText speech) of
                Just sentence ->
                    if model.currentCharacter == String.length sentence then
                        case Array.get model.currentSentence (speechText speech) of
                            Just nextSentence ->
                                ( { model
                                    | currentSentence = model.currentSentence + 1
                                    , currentCharacter = 0
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                update StepForward model
                    else
                        ( { model | currentCharacter = model.currentCharacter + 1 }, Cmd.none )

                Nothing ->
                    update StepForward model

        ( _, _ ) ->
            ( model, Cmd.none )


dogeFace : Form
dogeFace =
    image 64 64 "doge.gif"
        |> toForm


dialogueSnippet : Int -> String -> String
dialogueSnippet end sentence =
    String.slice 0 end sentence


characterView : Point -> Form
characterView point =
    circle 5
        |> filled Color.white
        |> move point


gameFrame : Model -> List Form -> List Form
gameFrame model game =
    List.append game
        [ (characterView model.player)
        , (move ( -160, 160 ) dogeFace)
        , rect 2 400
            |> filled Color.gray
            |> moveX -198
        , rect 2 400
            |> filled Color.gray
            |> moveX 198
        , rect 400 2
            |> filled Color.gray
            |> moveY -198
        , rect 400 2
            |> filled Color.gray
            |> moveY 198
        ]


gameEnemy : Enemy -> Form
gameEnemy enemy =
    enemy.text
        |> fromString
        |> monospace
        |> color enemy.color
        |> height 20
        |> Element.centered
        |> toForm
        |> move enemy.position


gameRound : RoundModel -> List Form
gameRound round =
    List.map gameEnemy round.enemies


timerView : Int -> Form
timerView ticks =
    30
        - ticks
        |> toString
        |> fromString
        |> color Color.gray
        |> Element.centered
        |> toForm
        |> move ( 180, 180 )


gameView : Model -> List Form
gameView model =
    case model.step of
        SpeechStep speech ->
            case Array.get model.currentSentence (speechText speech) of
                Just sentence ->
                    gameFrame model
                        [ dialogueSnippet model.currentCharacter sentence
                            |> fromString
                            |> color Color.white
                            |> monospace
                            |> Element.leftAligned
                            |> toForm
                            |> moveY 160
                        ]

                Nothing ->
                    gameFrame model []

        RoundStep round ->
            (roundModel round)
                |> gameRound
                |> gameFrame model
                |> List.append [ timerView model.roundTicks ]

        _ ->
            gameFrame model []


shouldAdvance : Model -> Bool
shouldAdvance model =
    case model.step of
        Finish ->
            True

        _ ->
            False


view : Model -> Html Msg
view model =
    collage
        400
        400
        (gameView model)
        |> Element.toHtml


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        stepSub =
            case model.step of
                SpeechStep _ ->
                    Time.every (175 * Time.millisecond) Speak

                RoundStep _ ->
                    Time.every Time.second RoundTick

                Finish ->
                    Sub.none
    in
        Sub.batch
            [ Keyboard.downs KeyDown
            , Keyboard.ups KeyUp
            , AnimationFrame.diffs Frame
            , stepSub
            ]
