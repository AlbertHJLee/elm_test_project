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

import Svg exposing (svg, circle, rect, line, polygon, text_)
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
        3 -> scene_three model
        _ -> div [] []
    ]




-- SCENES


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


triangle1 data =
  let
    translate_text =
      "translate(" ++ (String.fromFloat data.x) ++ "," ++ (String.fromFloat data.y) ++
      ") rotate(" ++ (String.fromFloat data.angle) ++ " 40,30)"
  in
  polygon
    [ points "0,0 80,60 0,60"
    , transform translate_text
    , fill "#c0c0ff"
    , fillOpacity (String.fromFloat data.opacity)
    , stroke "#0000f0"
    , strokeWidth "2"
    , strokeOpacity (String.fromFloat data.opacity)
    , strokeLinejoin "round"
    ]
    []


scene_zero : Model -> Html Msg
scene_zero model =
  let
    diff = (Time.posixToMillis model.current_time) - (Time.posixToMillis model.click_time)
    t = (toFloat diff) * 0.001
    -- Fade in first triangle
    ease_1 = transition 0.4 2.0 ( \theta -> theta ) t
  in
  div
    []
    [svg
      [ width "600"
      , height "500"
      ]
      [ triangle1 { x = 260, y = 300, angle = 0.0, opacity = ease_1 }
      ]
    ]


scene_one : Model -> Html Msg
scene_one model =

  let
    diff = (Time.posixToMillis model.current_time) - (Time.posixToMillis model.click_time)
    seconds_elapsed = (toFloat diff) * 0.001

    -- Rotate first triangle
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

    -- Move next two triangles into position
    ease_2 = transition 1.6 2.6 ( Ease.bezier 0.26 0.79 0.28 1.00 ) seconds_elapsed
    ease_3 = transition 1.9 2.9 ( Ease.bezier 0.26 0.79 0.28 1.00 ) seconds_elapsed
    ease_4 = transition 3.2 5.2 ( Ease.bezier 0.79 0.01 0.21 1.00 ) seconds_elapsed

    -- First half of triangle 3's motion
    x_j = 274
    y_j = 160
    x_b1 = interpolate x_i x_j ease_2
    y_b1 = interpolate y_i y_j ease_2
    -- First half of triangle 4's motion
    x_k = 170
    y_k = 170
    x_c1 = interpolate x_i x_k ease_3
    y_c1 = interpolate y_i y_k ease_3
    x_l = 332
    y_l = 228

    -- Second half of both triangle 3 and 4's motions
    x_b2 = interpolate 0.0 (x_l - x_j) ease_4
    y_b2 = interpolate 0.0 (y_l - y_j + 1) ease_4
    x_c2 = interpolate 0.0 (x_l - x_k - 1) ease_4
    y_c2 = interpolate 0.0 (y_l - y_k) ease_4
    x_b = x_b1 + x_b2
    y_b = y_b1 + y_b2
    x_c = x_c1 + x_c2
    y_c = y_c1 + y_c2
    angle_b = interpolate 0.0 -90.0 ease_4
    angle_c = interpolate 0.0 90.0 ease_4

  in
  div
    [ ]
    [ svg
        [ width "600"
        , height "500"
        ]
        [ triangle1 { x = x_i, y = y_i, angle = 0.0, opacity = 1.0 }
        , triangle1 { x = x_c, y = y_c, angle = angle_c, opacity = 0.5 }
        , triangle1 { x = x_b, y = y_b, angle = angle_b, opacity = 0.5 }
        , triangle1 { x = x_a, y = y_a, angle = angle, opacity = 0.5 }
        ]
    ]


triangles_paired theta =
  let
    dx = 30.0 * theta
    x_i = 260 - dx
    y_i = 300
    x_a = x_i + 1
    y_a = y_i - 1
    x_b = x_i + (332-260)
    y_b = y_i + (228-300) + 1
    x_c = x_b - 1
    y_c = y_b - 1
    op_a = interpolate 0.5 1.0 theta
  in
  [ triangle1 { x = x_i, y = y_i, angle = 0.0, opacity = 1.0 }
  , triangle1 { x = x_c, y = y_c, angle = 90.0, opacity = op_a }
  , triangle1 { x = x_b, y = y_b, angle = -90.0, opacity = op_a }
  , triangle1 { x = x_a, y = y_a, angle = 180.0, opacity = op_a }
  ]


square1 data =
  let
    translate_text =
      "translate(" ++ (String.fromFloat data.x) ++ "," ++ (String.fromFloat data.y) ++
      ") rotate(" ++ (String.fromFloat data.angle) ++ " 0,0)"
    side_length = (String.fromFloat data.s)
  in
  rect
    [ width side_length
    , height side_length
    , transform translate_text
    , fill data.color
    , fillOpacity (String.fromFloat data.opacity)
    , stroke data.color
    , strokeOpacity (String.fromFloat data.opacity)
    , strokeWidth "2"
    , strokeLinejoin "round"
    ]
    []


text1 data =
  text_
    [ x (String.fromFloat data.x)
    , y (String.fromFloat data.y)
    , textAnchor "middle"        -- Center the text horizontally
    , dominantBaseline "middle"  -- Center the text vertically
    , fontSize data.fontsize
    , opacity (String.fromFloat data.opacity)
    ]
    data.texts


scene_two model =

  let
    diff = (Time.posixToMillis model.current_time) - (Time.posixToMillis model.click_time)
    t = (toFloat diff) * 0.001

    -- Recenter triangles
    ease_1 = transition 0.2 1.2 ( Ease.bezier 0.26 0.79 0.28 1.00 ) t

    -- Fade in squares
    ease_2 = transition 1.6 2.6 ( Ease.bezier 0.26 0.79 0.28 1.00 ) t
    sq1 = { x = (260 - 30), y = (300 - 80 - 2), s = 80 }
    sq2 = { x = (sq1.x + 80 + 3), y = 300, s = 60 }

    -- Fade in text
    ease_3 = transition 3.2 3.6 ( Ease.bezier 0.26 0.79 0.28 1.00 ) t
    txt1 = { x = (sq1.x + sq1.s / 2.0), y = (sq1.y + sq1.s / 2.0) }
    txt2 = { x = (sq2.x + sq2.s / 2.0), y = (sq2.y + sq2.s / 2.0) }
    texts1 = [ Svg.text "b"
             , Svg.tspan [ fontSize "11", dy "-8" ] [ Svg.text "2" ] ]
    texts2 = [ Svg.text "a"
             , Svg.tspan [ fontSize "11", dy "-8" ] [ Svg.text "2" ] ]
  in
  div
    []
    [ svg
        [ width "600"
        , height "500"
        ]
        (
          [ square1 { x = (260 - 30), y = (300 - 80 - 2), s = 80, angle = 0.0, opacity = ease_2, color = "#ffc0c0" }
          , square1 { x = (260 + 80 + 3 - 30), y = 300, s = 60, angle = 0.0, opacity = ease_2, color = "#c0ffc0" }
          ] ++
          ( triangles_paired ease_1 ) ++
          [ text1 { x = txt1.x, y = txt1.y, texts = texts1, opacity = ease_3, fontsize = "20px" }
          , text1 { x = txt2.x, y = txt2.y, texts = texts2, opacity = ease_3, fontsize = "20px" }
          ]
        )
    ]


scene_three model =

  let
    diff = (Time.posixToMillis model.current_time) - (Time.posixToMillis model.click_time)
    t = (toFloat diff) * 0.001

    -- Fade out squares
    ease_1 = transition 0.5 1.5 ( Ease.bezier 0.26 0.79 0.28 1.00 ) t
    op1 = interpolate 1.0 0.0 ease_1
    sq1 = { x = (260 - 30), y = (300 - 80 - 2), s = 80 }
    sq2 = { x = (sq1.x + 80 + 3), y = 300, s = 60 }

    -- Fade out text
    txt1 = { x = (sq1.x + sq1.s / 2.0), y = (sq1.y + sq1.s / 2.0) }
    txt2 = { x = (sq2.x + sq2.s / 2.0), y = (sq2.y + sq2.s / 2.0) }
    texts1 = [ Svg.text "b"
             , Svg.tspan [ fontSize "11", dy "-8" ] [ Svg.text "2" ] ]
    texts2 = [ Svg.text "a"
             , Svg.tspan [ fontSize "11", dy "-8" ] [ Svg.text "2" ] ]
  in
  div
    []
    [ svg
        [ width "600"
        , height "500"
        ]
        (
          [ square1 { x = (260 - 30), y = (300 - 80 - 2), s = 80, angle = 0.0, opacity = op1, color = "#ffc0c0" }
          , square1 { x = (260 + 80 + 3 - 30), y = 300, s = 60, angle = 0.0, opacity = op1, color = "#c0ffc0" }
          ] ++
          [ text1 { x = txt1.x, y = txt1.y, texts = texts1, opacity = op1, fontsize = "20px" }
          , text1 { x = txt2.x, y = txt2.y, texts = texts2, opacity = op1, fontsize = "20px" }
          ] ++
          ( triangles_paired 1.0 )
        )
    ]
