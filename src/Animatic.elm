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

scene_one : Model -> Html Msg
scene_one model =
  let
    diff = (Time.posixToMillis model.current_time) - (Time.posixToMillis model.click_time)
    fdiff = (toFloat diff) * 0.001
    t1 = 1.6
    ease_input =
      if (fdiff < t1) then (fdiff / t1)
      else 1.0
    -- ease_output = Ease.bezier 0.16 0.15 0.11 1.00 ease_input
    -- ease_output = Ease.bezier 0.20 0.00 0.20 1.00 ease_input
    -- ease_output = Ease.bezier 0.50 0.01 0.50 1.00 ease_input
    -- ease_output = Ease.bezier 0.47 0.14 0.03 0.79 ease_input
    ease_output = Ease.bezier 0.26 0.79 0.28 1.00 ease_input
    x0 = 260
    y0 = 300
    x1 = 261
    y1 = 299
    x = (x0 + (x1 - x0) * ease_output)
    y = (y0 + (y1 - y0) * ease_output)
    -- rx = x0 + (0.5 * w0)
    -- ry = y0 + (0.5 * h0)
    angle = 180.0 * ease_output
  in
  div
    [ ]
    [ svg
        [ width "600"
        , height "500"
        ]
        [ triangle1 { x = x0, y = y0, angle = 0.0, opacity=1.0 }
        , triangle1 { x = x, y = y, angle = angle, opacity=0.5 }
        ]
    ]

scene_two model =
  div [] []
