module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Html exposing (Html, sub)
import Json.Decode as Decode
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (cx, cy, height, r, viewBox, width, x, y)


type alias Model =
    { ball : Ball
    , rightPaddle : Paddle
    , leftPaddle : Paddle
    }


type Paddle
    = LeftPaddle PaddleInfo
    | RightPaddle PaddleInfo


type alias PaddleInfo =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


type alias Ball =
    { x : Int
    , y : Int
    , radious : Int
    , speedX : Int
    }


initBall : Ball
initBall =
    { x = 250
    , y = 260
    , radious = 10
    , speedX = 4
    }


initPaddle : Int -> PaddleInfo
initPaddle initaialX =
    { x = initaialX
    , y = 225
    , width = 10
    , height = 50
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { ball = initBall
      , rightPaddle = RightPaddle <| initPaddle 480
      , leftPaddle = LeftPaddle <| initPaddle 10
      }
    , Cmd.none
    )


type Msg
    = OnAnimationFrame Float
    | KeyDown PlayerAction


type PlayerAction
    = RightPaddleUp
    | RightPaddleDown
    | LeftPaddleUp
    | LeftPaddleDown


type alias Flags =
    ()


subscriptions : Model -> Sub Msg
subscriptions _ =
    -- Browser.Events.onAnimationFrameDelta OnAnimationFrame
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta OnAnimationFrame
        , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        ]


keyDecoder : Decode.Decoder PlayerAction
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen keyToPlayerAction


keyToPlayerAction : String -> Decode.Decoder PlayerAction
keyToPlayerAction keyString =
    case keyString of
        "ArrowUp" ->
            Decode.succeed RightPaddleUp

        "ArrowDown" ->
            Decode.succeed RightPaddleDown

        "e" ->
            Decode.succeed LeftPaddleUp

        "d" ->
            Decode.succeed LeftPaddleDown

        _ ->
            Decode.fail "not a event we care about"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAnimationFrame timeDelta ->
            let
                ball =
                    model.ball

                shouldBounce =
                    shouldBallBounce model.rightPaddle model.ball
                        || shouldBallBounce model.leftPaddle model.ball

                speedX =
                    if shouldBounce then
                        ball.speedX * -1

                    else
                        ball.speedX

                updatedBall =
                    { ball | x = ball.x + speedX, speedX = speedX }
            in
            ( { model | ball = updatedBall }, Cmd.none )

        KeyDown playerAction ->
            case playerAction of
                RightPaddleUp ->
                    ( { model | rightPaddle = model.rightPaddle |> updatePaddle -10 }
                    , Cmd.none
                    )

                RightPaddleDown ->
                    ( { model | rightPaddle = model.rightPaddle |> updatePaddle 10 }
                    , Cmd.none
                    )

                LeftPaddleUp ->
                    ( { model | leftPaddle = model.leftPaddle |> updatePaddle -10 }
                    , Cmd.none
                    )

                LeftPaddleDown ->
                    ( { model | leftPaddle = model.leftPaddle |> updatePaddle 10 }
                    , Cmd.none
                    )


updatePaddle : Int -> Paddle -> Paddle
updatePaddle amount paddle =
    case paddle of
        RightPaddle paddleInfo ->
            { paddleInfo | y = paddleInfo.y + amount } |> RightPaddle

        LeftPaddle paddleInfo ->
            { paddleInfo | y = paddleInfo.y + amount } |> LeftPaddle


shouldBallBounce : Paddle -> Ball -> Bool
shouldBallBounce paddle ball =
    case paddle of
        LeftPaddle { x, y, width, height } ->
            (ball.x - ball.radious <= x + width)
                && (ball.y >= y)
                && (ball.y <= y + 50)

        RightPaddle { x, y, width, height } ->
            (ball.x + ball.radious >= x)
                && (ball.y >= y)
                && (ball.y <= y + 50)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Svg.Svg Msg
view { ball, rightPaddle, leftPaddle } =
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #efefef"
        ]
        [ viewBall ball
        , viewPaddle rightPaddle
        , viewPaddle leftPaddle
        ]


viewPaddle : Paddle -> Svg.Svg Msg
viewPaddle paddle =
    let
        paddleInfo =
            case paddle of
                LeftPaddle info ->
                    info

                RightPaddle info ->
                    info
    in
    rect
        [ x <| String.fromInt paddleInfo.x
        , y <| String.fromInt paddleInfo.y
        , width <| String.fromInt paddleInfo.width
        , height <| String.fromInt paddleInfo.height
        ]
        []


viewBall : Ball -> Svg.Svg Msg
viewBall ball =
    circle
        [ cx <| String.fromInt ball.x
        , cy <| String.fromInt ball.y
        , r <| String.fromInt ball.radious
        ]
        []
