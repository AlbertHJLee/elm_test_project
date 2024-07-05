--
-- Click through an animation of a geometry concept
--

module Animatic exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, button, div, text, h1, h2)
import Html.Events exposing (onClick)
import Html.Attributes as Attr
import Time
import Task

import Svg exposing (svg, circle, rect, line, polygon)
import Svg.Attributes exposing (..)
import Ease




-- MAIN

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }




-- MODEL


type alias Model =
  { index : Int
  , max_scenes : Int
  , click_time : Time.Posix
  , current_time : Time.Posix
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model
      0
      4
      (Time.millisToPosix 0)
      (Time.millisToPosix 0)
  , Task.perform SetTime Time.now
  )




-- UPDATE


type Msg
  = Previous
  | Next
  | SetTime Time.Posix
  | Tick Time.Posix
  | Reset


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    Next ->
      if model.index == model.max_scenes then
        ( model
        , Cmd.none
        )
      else
        ( { model | index = model.index + 1 }
        , Task.perform SetTime Time.now
        )

    Previous ->
      if model.index == 0 then
        ( model
        , Cmd.none
        )
      else
        ( { model | index = model.index - 1 }
        , Task.perform SetTime Time.now
        )

    SetTime newTime ->
      ( { model
          | click_time = newTime
          , current_time = newTime
        }
      , Cmd.none
      )

    Tick newTime ->
      ( { model
          | current_time = newTime
        }
      , Cmd.none
      )

    Reset ->
      ( { model
          | index = 0
        }
      , Task.perform SetTime Time.now
      )


subscriptions : Model -> Sub Msg
subscriptions model =
  Browser.Events.onAnimationFrame Tick




-- VIEW


view : Model -> Html Msg
view model =
  let
    x = 0
  in
  div
    [ Attr.style "width" "device-width"
    , Attr.style "height" "100vh"
    , Attr.style "display" "flex"
    , Attr.style "justify-content" "center"
    , Attr.style "align-items" "center"
    , Attr.style "background-color" "gray"
    , Attr.style "border" "3px solid black"
    ]
    [ div
        [ Attr.style "width" "640px"
        , Attr.style "height" "840px"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "background-color" "yellow"
        ]
        [ div
            [ Attr.style "width" "600px"
            , Attr.style "height" "540px"
            , Attr.style "background-color" "white"
            , Attr.style "margin" "10px auto"
            ]
            [ viewScene model]
        , div
            [ Attr.style "width" "620px"
            , Attr.style "height" "10px"
            , Attr.style "background-color" "red"
            , Attr.style "margin" "10px auto"
            ]
            []
        , div
            [ Attr.style "width" "160px"
            , Attr.style "background-color" "green"
            , Attr.style "margin" "10px auto"
            , Attr.style "text-align" "center"
            ]
            [ div
                []
                [ h2 [] [ text "Index" ]
                ]
            , div
                [ Attr.style "width" "100%"
                , Attr.style "height" "30px"
                , Attr.style "margin-bottom" "10px"
                , Attr.style "padding-top" "6px"
                , Attr.style "align-items" "center"
                , Attr.style "background-color" "white"
                ]
                [ text (String.fromInt model.index) ]
            , button [ onClick Previous ] [ text "Previous" ]
            , button [ onClick Next ] [ text "Next" ]
            , div [] []
            , button [ onClick Reset] [ text "reset" ]
            ]
        ]
    ]


viewScene : Model -> Html Msg
viewScene model =
  div
    []
    [ case model.index of
        0 -> scene_zero model
        1 -> scene_one model
        2 -> scene_two model
        _ -> div [] []
    ]




-- SCENES

scene_zero : Model -> Html Msg
scene_zero model =
  let
    diff = (Time.posixToMillis model.current_time) - (Time.posixToMillis model.click_time)
    fdiff = (toFloat diff) * 0.001
    t0 = 0.4
    op_val =
      if (fdiff < t0) then 0.0
      else if (fdiff < 2.0) then (fdiff - t0)
      else 1.0
  in
  div
    []
    [svg
      [ width "600"
      , height "500"
      ]
      [ triangle1 { x = 260, y = 300, angle = 0.0, opacity = op_val }
      ]
    ]

triangle1 data =
  let
    translate_text =
      "translate(" ++ (String.fromFloat data.x) ++ "," ++ (String.fromFloat data.y) ++
      ") rotate(" ++ (String.fromFloat data.angle) ++ " 40,30)"
  in
  polygon
    [ points "0,0 80,60 0,60"
    -- , transform "translate(260,300)"
    , transform translate_text
    , fill "#c0c0ff"
    , fillOpacity (String.fromFloat data.opacity)
    , stroke "#0000f0"
    , strokeWidth "2"
    , strokeOpacity (String.fromFloat data.opacity)
    ]
    []

transition : Float -> Float -> ( Float -> Float) -> Float -> Float
transition t1 t2 fxn t =
  let
    duration = t2 - t1
    ease_input =
      if (t < t1) then 0.0
      else if (t < t2) then (t - t1) / duration
      else 1.0
    ease_output = fxn ease_input
  in
  ease_output

interpolate : Float -> Float -> Float -> Float
interpolate x1 x2 theta =
  x1 + (x2 - x1) * theta


scene_one : Model -> Html Msg
scene_one model =
  let
    diff = (Time.posixToMillis model.current_time) - (Time.posixToMillis model.click_time)
    seconds_elapsed = (toFloat diff) * 0.001
    ease_1 =
      transition
        0.2
        1.6
        ( Ease.bezier 0.26 0.79 0.28 1.00 )
        seconds_elapsed
    x_i = 260
    y_i = 300
    x_a = interpolate x_i (x_i+1) ease_1
    y_a = interpolate y_i (y_i-1) ease_1
    angle = interpolate 0.0 180.0 ease_1
    ease_2 = transition 1.6 2.6 ( Ease.bezier 0.26 0.79 0.28 1.00 ) seconds_elapsed
    x_j = 274
    y_j = 160
    x_b = interpolate x_i x_j ease_2
    y_b = interpolate y_i y_j ease_2
    ease_3 = transition 1.9 2.9 ( Ease.bezier 0.26 0.79 0.28 1.00 ) seconds_elapsed
    x_k = 170
    y_k = 170
    x_c = interpolate x_i x_k ease_3
    y_c = interpolate y_i y_k ease_3
  in
  div
    [ ]
    [ svg
        [ width "600"
        , height "500"
        ]
        [ triangle1 { x = x_i, y = y_i, angle = 0.0, opacity=1.0 }
        , triangle1 { x = x_c, y = y_c, angle = 0.0, opacity=0.5 }
        , triangle1 { x = x_b, y = y_b, angle = 0.0, opacity=0.5 }
        , triangle1 { x = x_a, y = y_a, angle = angle, opacity=0.5 }
        ]
    ]

scene_two model =
  div [] []
