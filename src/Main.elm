module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Html exposing (Html, sub)
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (cx, cy, height, r, viewBox, width, x, y)


type alias Model =
    Ball


type alias Ball =
    { x : Int
    , y : Int
    , radious : Int
    , speedX : Int
    }


ball =
    { x = 250, y = 250, radious = 10, speedX = 4 }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( ball
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
            ( { model | x = model.x + model.speedX }, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Ball -> Svg.Svg Msg
view model =
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #efefef"
        ]
        [ viewBall model
        , rect
            [ x "480"
            , y "225"
            , width "10"
            , height "50"
            ]
            []
        ]


viewBall : Ball -> Svg.Svg Msg
viewBall { x, y, radious } =
    circle
        [ cx <| String.fromInt x
        , cy <| String.fromInt y
        , r <| String.fromInt radious
        ]
        []
