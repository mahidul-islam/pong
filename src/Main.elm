module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Html exposing (Html, sub)
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (cx, cy, height, r, viewBox, width, x, y)


type alias Model =
    { ball : Ball
    , paddle : Paddle
    }


type alias Paddle =
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


initPaddle : Paddle
initPaddle =
    { x = 480
    , y = 225
    , width = 10
    , height = 50
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { ball = initBall
      , paddle = initPaddle
      }
    , Cmd.none
    )


type Msg
    = OnAnimationFrame Float


type alias Flags =
    ()


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta OnAnimationFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAnimationFrame timeDelta ->
            let
                ball =
                    model.ball

                shouldBounce =
                    shouldBallBounce model.paddle model.ball

                speedX =
                    if shouldBounce then
                        ball.speedX * -1

                    else
                        ball.speedX

                updatedBall =
                    { ball | x = ball.x + speedX, speedX = speedX }
            in
            ( { model | ball = updatedBall }, Cmd.none )


shouldBallBounce : Paddle -> Ball -> Bool
shouldBallBounce paddle ball =
    (ball.x + ball.radious >= paddle.x)
        && (ball.y >= paddle.y)
        && (ball.y <= paddle.y + 50)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Svg.Svg Msg
view { ball, paddle } =
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #efefef"
        ]
        [ viewBall ball
        , viewPaddle paddle
        ]


viewPaddle : Paddle -> Svg.Svg Msg
viewPaddle paddle =
    rect
        [ x <| String.fromInt paddle.x
        , y <| String.fromInt paddle.y
        , width <| String.fromInt paddle.width
        , height <| String.fromInt paddle.height
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
