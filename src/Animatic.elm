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


-- The model should track both the time an event was triggered (click_time)
-- and the current time. Thus, animations can be timed relative to the event

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
    -- Window dimensions
    window_width = 600
    window_height = 540
    window_half_stroke = 1
    window_stroke = window_half_stroke * 2
    win_stroke_text = ((String.fromInt window_stroke) ++ "px solid #f0f0f0")

    -- Spacing for control area
    window_margin_bottom = 20 - window_stroke
    input_area_height = 260

    -- Make room for border strokes, which get added to outside of div box
    panel_width = window_width + 4 * window_half_stroke
    panel_height = window_height + input_area_height + 4 * window_half_stroke
    panel_padding_visual = 30
    panel_padding = panel_padding_visual - 2 * window_half_stroke

    -- Control Box
    -- See viewControls function below

  in
  div
    [ Attr.style "width" "device-width"
    , Attr.style "height" "100vh"
    , Attr.style "display" "flex"
    , Attr.style "justify-content" "center"
    , Attr.style "align-items" "center"
    , Attr.style "background-color" "#f0f0f0"
    ]
    [ div
        [ Attr.style "width" ((String.fromInt panel_width) ++ "px")
        , Attr.style "height" ((String.fromInt panel_height) ++"px")
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "background-color" "white"
        , Attr.style "padding" ((String.fromInt panel_padding) ++ "px")
        , Attr.style "border-radius" "16px"
        ]
        [
          -- Designated area for viewing scenes and visuals
          div
            [ Attr.style "width" ((String.fromInt window_width) ++ "px")
            , Attr.style "height" ((String.fromInt window_height) ++ "px")
            , Attr.style "background-color" "white"
            , Attr.style "margin" "auto"
            , Attr.style "margin-bottom" ((String.fromInt window_margin_bottom) ++ "px")
            , Attr.style "border" win_stroke_text
            , Attr.style "border-radius" "6px"
            ]
            [ viewScene model]
        ,
          -- Placeholder for UI element
          div
            [ Attr.style "width" ((String.fromInt window_width) ++ "px")
            , Attr.style "height" "10px"
            , Attr.style "margin" "auto"
            ]
            []
        ,
          -- Box holding main user controls
          div
            []
            [ viewControls model ]
        ]
    ]


viewControls : Model -> Html Msg
viewControls model =
  let
    controls_font = "20px"
  in
  div
    [ Attr.style "width" "160px"
    , Attr.style "margin" "10px auto"
    , Attr.style "text-align" "center"
    , Attr.style "font-size" controls_font
    , Attr.style "background-color" "white"
    ]
    [ div [] [ text "Index" ]
    , div
        [ Attr.style "width" "100%"
        , Attr.style "height" "36px"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        ]
        [ text (String.fromInt model.index) ]
    , button [ onClick Previous, Attr.style "font-size" controls_font ] [ text "<" ]
    , button [ onClick Next,     Attr.style "font-size" controls_font ] [ text ">" ]
    , div [] []
    , button [ onClick Reset,    Attr.style "font-size" controls_font ] [ text "reset" ]
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
        4 -> scene_four model
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


triangle_svg data =
  let
    transform_text =
      "translate(" ++ (String.fromFloat data.x) ++ "," ++ (String.fromFloat data.y) ++
      ") rotate(" ++ (String.fromFloat data.angle) ++ " 40,30)"
  in
  polygon
    [ points "0,0 80,60 0,60"
    , transform transform_text
    , fill "#c0c0ff"
    , fillOpacity (String.fromFloat data.opacity)
    , stroke "#0000f0"
    , strokeWidth "2"
    , strokeOpacity (String.fromFloat data.opacity)
    , strokeLinejoin "round"
    ]
    []


one_line xa ya xb yb opac transform_text =
  line
    [ x1 (String.fromFloat xa)
    , y1 (String.fromFloat ya)
    , x2 (String.fromFloat xb)
    , y2 (String.fromFloat yb)
    , stroke "#0000f0"
    , strokeWidth "2"
    , opacity (String.fromFloat opac)
    , transform transform_text
    ]
    []

congruency_marks data =
  let
    xc = data.x
    yc = data.y
    angle = data.angle
    halflength = 8
    transform_text =
      "rotate(" ++ (String.fromFloat data.angle) ++
      " " ++ (String.fromFloat xc) ++ "," ++ (String.fromFloat yc) ++ ")"

    -- Generate list of x-coordinates for n marks
    mark_spacing = 4
    offset = mark_spacing * ( data.n / 2 + 0.5 )
    mark_list =
      ( List.range 1 (round data.n) )
          |> ( List.map (\p -> (toFloat p) * mark_spacing - offset + xc ) )
  in
  -- Pipe list of x-coordinates into function for drawing a single mark
  ( List.map
      ( \xi -> ( one_line xi (yc + halflength) xi (yc - halflength) data.opacity transform_text ) )
      mark_list
  )


ninety data =
  let
    xc = data.x
    yc = data.y
    angle = data.angle
    transform_text =
      "rotate(" ++ (String.fromFloat data.angle) ++
      " " ++ (String.fromFloat xc) ++ "," ++ (String.fromFloat yc) ++ ")"
    length = 10
  in
  [ ( one_line (xc + length) yc (xc + length) (yc - length - 1) data.opacity transform_text )
  , ( one_line xc (yc - length) (xc + length + 1) (yc - length) data.opacity transform_text )
  ]


scene_zero : Model -> Html Msg
scene_zero model =
  let
    diff = (Time.posixToMillis model.current_time) - (Time.posixToMillis model.click_time)
    t = (toFloat diff) * 0.001

    -- Calculate times
    t1 = 0.6
    t2 = t1 + 0.4
    t3 = t2 + 0.1  -- fade in annotations
    t4 = t3 + 0.15

    -- Fade in first triangle
    ease_1 = transition t1 t2 ( \theta -> theta ) t

    -- Calculate annotation positions
    side_a = 60.0
    side_b = 80.0
    xo = 260.0
    yo = 300.0
    xa = xo
    ya = yo + (side_a / 2)
    xb = xo + (side_b / 2)
    yb = yo + side_a
    xc = xo + (side_b / 2)
    yc = yo + (side_a / 2)
    anglec = ( atan (3.0/4.0) ) / pi * 180

    dt = 0.3
    ease_2 = transition t3 t4 ( \theta -> theta ) t
    ease_3 = transition (t3 + dt) (t4 + dt) ( \theta -> theta ) t
    ease_4 = transition (t3 + 2*dt) (t4 + 2*dt) ( \theta -> theta ) t

    txt_spacing = 24
    txt1 = { x = xa - txt_spacing, y = ya }
    texts1 = [ text "a" ]
    txt2 = { x = xb, y = yb + txt_spacing }
    texts2 = [ text "b" ]
    txt3 = { x = xc + txt_spacing * (3/5), y = yc - txt_spacing * (4/5) }
    texts3 = [ text "c" ]

  in
  div
    []
    [svg
      [ width "600"
      , height "500"
      ]
      (
      [ triangle_svg { x = xo, y = yo, angle = 0.0, opacity = ease_1 } ] ++
      ( congruency_marks { x = xa, y = ya, angle =  -90.0, opacity = ease_2, n = 1 } ) ++
      ( congruency_marks { x = xb, y = yb, angle =    0.0, opacity = ease_3, n = 2 } ) ++
      ( congruency_marks { x = xc, y = yc, angle = anglec, opacity = ease_4, n = 3 } ) ++
      ( ninety { x = xo, y = yo + side_a, angle = 0.0, opacity = ease_1 } ) ++
      [ text_svg { x = txt1.x, y = txt1.y, texts = texts1, opacity = ease_2, fontsize = "20px" }
      , text_svg { x = txt2.x, y = txt2.y, texts = texts2, opacity = ease_3, fontsize = "20px" }
      , text_svg { x = txt3.x, y = txt3.y, texts = texts3, opacity = ease_4, fontsize = "20px" }
      ]
      )
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
        [ triangle_svg { x = x_i, y = y_i, angle = 0.0, opacity = 1.0 }
        , triangle_svg { x = x_c, y = y_c, angle = angle_c, opacity = 0.5 }
        , triangle_svg { x = x_b, y = y_b, angle = angle_b, opacity = 0.5 }
        , triangle_svg { x = x_a, y = y_a, angle = angle, opacity = 0.5 }
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
  [ triangle_svg { x = x_i, y = y_i, angle = 0.0, opacity = 1.0 }
  , triangle_svg { x = x_c, y = y_c, angle = 90.0, opacity = op_a }
  , triangle_svg { x = x_b, y = y_b, angle = -90.0, opacity = op_a }
  , triangle_svg { x = x_a, y = y_a, angle = 180.0, opacity = op_a }
  ]


square_svg data =
  let
    side_length = String.fromFloat data.s
    halfside = String.fromFloat (data.s/2.0)
    transform_text =
      "translate(" ++ (String.fromFloat data.x) ++ "," ++ (String.fromFloat data.y) ++
      ") rotate(" ++ (String.fromFloat data.angle) ++
      " " ++ halfside ++ "," ++ halfside ++ ")"
  in
  rect
    [ width side_length
    , height side_length
    , transform transform_text
    , fill data.color
    , fillOpacity (String.fromFloat data.opacity)
    , stroke data.color
    , strokeOpacity (String.fromFloat data.opacity)
    , strokeWidth "2"
    , strokeLinejoin "round"
    ]
    []


text_svg data =
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
          -- Render squares before triangles so that triangle strokes are not obscured
          [ square_svg { x = sq1.x, y = sq1.y, s = sq1.s, angle = 0.0, opacity = ease_2, color = "#ffc0c0" }
          , square_svg { x = sq2.x, y = sq2.y, s = sq2.s, angle = 0.0, opacity = ease_2, color = "#c0ffc0" }
          ] ++
          ( triangles_paired ease_1 ) ++
          [ text_svg { x = txt1.x, y = txt1.y, texts = texts1, opacity = ease_3, fontsize = "20px" }
          , text_svg { x = txt2.x, y = txt2.y, texts = texts2, opacity = ease_3, fontsize = "20px" }
          ]
        )
    ]


scene_three model =
  let
    diff = (Time.posixToMillis model.current_time) - (Time.posixToMillis model.click_time)
    t = (toFloat diff) * 0.001

    -- Calculate key times
    t0 = 0.7       -- wipe in background
    t1 = t0 + 1.2
    t2 = t1 + 0.5  -- fade out squares
    t3 = t2 + 0.8
    t4 = t3 + 0.8  -- first two triangles
    t5 = t4 + 1.0
    t6 = t5 + 0.2  -- last triangle
    t7 = t6 + 1.0

    -- Fade out squares
    ease_2 = transition t2 t3 ( Ease.bezier 0.26 0.79 0.28 1.00 ) t
    op2 = interpolate 1.0 0.0 ease_2
    sq1 = { x = (260 - 30), y = (300 - 80 - 2), s = 80 }
    sq2 = { x = (sq1.x + 80 + 3), y = 300, s = 60 }

    -- Fade out text
    txt1 = { x = (sq1.x + sq1.s / 2.0), y = (sq1.y + sq1.s / 2.0) }
    txt2 = { x = (sq2.x + sq2.s / 2.0), y = (sq2.y + sq2.s / 2.0) }
    texts1 = [ Svg.text "b"
             , Svg.tspan [ fontSize "11", dy "-8" ] [ Svg.text "2" ] ]
    texts2 = [ Svg.text "a"
             , Svg.tspan [ fontSize "11", dy "-8" ] [ Svg.text "2" ] ]

    -- Wipe in background (occurs before fade but references sq1)
    sq3 = { x = (sq1.x - 30), y = (sq1.y - 30), s = (80 + 60 + 3 + 60) }
    sq4 = { x = sq1.x, y = sq1.y, s = (80 + 60 + 2) }
    ease_1 = transition t0 t1 ( Ease.bezier 0.26 0.79 0.28 1.00 ) t
    sq5y = interpolate (sq3.y - 13) (sq3.y + sq3.s + 62) ease_1

    -- Move triangles
    x_i = 260 - 30
    y_i = 300
    --
    x_a1 = x_i + 1
    y_a1 = y_i - 1
    --
    x_b = x_i + (332-260)
    y_b1 = y_i + (228-300) + 1
    --
    x_c1 = x_b - 1
    y_c = y_b1 - 1
    --
    ease_3 = transition t4 t5 ( Ease.bezier 0.26 0.79 0.28 1.00 ) t
    ease_4 = transition t6 t7 ( Ease.bezier 0.26 0.79 0.28 1.00 ) t
    y_b = interpolate y_b1 (y_b1 + (60+1)) ease_3
    x_c = interpolate x_c1 (x_c1 - (80+1)) ease_3
    x_a = interpolate x_a1 (x_a1 + (60+1)) ease_4
    y_a = interpolate y_a1 (y_a1 - (80+1)) ease_4
    op_3 = 1.0

  in
  div
    []
    [ svg
        [ width "600"
        , height "500"
        ]
        (
          -- Add background squares to front of list so that they are rendered furthest back
          [ square_svg { x = sq3.x, y = sq3.y, s = sq3.s, angle = 0.0, opacity = 1.0, color = "#e0e0e0" }
          , square_svg { x = sq4.x, y = sq4.y, s = sq4.s, angle = 0.0, opacity = 1.0, color = "white" }
          , square_svg { x = sq3.x - 200, y = sq5y, s = sq3.s + 400, angle = -10.0, opacity = 1.0, color = "white" }
          ] ++
          -- Squares from previous animation are next on top
          [ square_svg { x = sq1.x, y = sq1.y, s = sq1.s, angle = 0.0, opacity = op2, color = "#ffc0c0" }
          , square_svg { x = sq2.x, y = sq2.y, s = sq2.s, angle = 0.0, opacity = op2, color = "#c0ffc0" }
          ] ++
          [ text_svg { x = txt1.x, y = txt1.y, texts = texts1, opacity = op2, fontsize = "20px" }
          , text_svg { x = txt2.x, y = txt2.y, texts = texts2, opacity = op2, fontsize = "20px" }
          ] ++
          -- Triangles are furthest forward
          [ triangle_svg { x = x_i, y = y_i, angle = 0.0, opacity = op_3 }
          , triangle_svg { x = x_c, y = y_c, angle = 90.0, opacity = op_3 }
          , triangle_svg { x = x_b, y = y_b, angle = -90.0, opacity = op_3 }
          , triangle_svg { x = x_a, y = y_a, angle = 180.0, opacity = op_3 }
          ]
        )
    ]


scene_four model =
  let
    diff = (Time.posixToMillis model.current_time) - (Time.posixToMillis model.click_time)
    t = (toFloat diff) * 0.001

    -- squares are fixed in location
    sq1 = { x = (260 - 30), y = (300 - 80 - 2), s = 80 }
    sq3 = { x = (sq1.x - 30), y = (sq1.y - 30), s = (80 + 60 + 3 + 60) }
    sq4 = { x = sq1.x, y = sq1.y, s = (80 + 60 + 2) }
    ease_1 = transition 0.8 1.8 ( Ease.bezier 0.26 0.79 0.28 1.00 ) t
    op5 = ease_1
    angle5 = (atan ( 3.0/4.0 )) * 180 / pi
    txt1 = { x = sq4.x + 70, y = sq4.y + 70 }
    texts1 = [ Svg.text "c"
             , Svg.tspan [ fontSize "11", dy "-8" ] [ Svg.text "2" ] ]

    -- new square
    sq5 = { x = sq4.x + (70-50), y = sq4.y + (70-50), s = 100 }

    -- triangles
    x_i = 260 - 30
    y_i = 300
    --
    x_a = x_i + 1 + 61
    y_a = y_i - 1 - 81
    --
    x_b = x_i + (332-260)
    y_b = y_i + (228-300) + 1 + 61
    --
    x_c = x_b - 1 - 81
    y_c = y_b - 1 - 61
    op_3 = 1.0

  in
  div
    []
    [ svg
        [ width "600"
        , height "500"
        ]
        (
          -- Background furthest back
          [ square_svg { x = sq3.x, y = sq3.y, s = sq3.s, angle = 0.0, opacity = 1.0, color = "#e0e0e0" }
          , square_svg { x = sq4.x, y = sq4.y, s = sq4.s, angle = 0.0, opacity = 1.0, color = "white" }
          ] ++
          -- New square
          [ square_svg { x = sq5.x, y = sq5.y, s = sq5.s, angle = angle5, opacity = op5, color = "yellow" }
          , text_svg { x = txt1.x, y = txt1.y, texts = texts1, opacity = op5, fontsize = "20px" }
          ] ++
          -- Triangles furthest forward
          [ triangle_svg { x = x_i, y = y_i, angle = 0.0, opacity = op_3 }
          , triangle_svg { x = x_c, y = y_c, angle = 90.0, opacity = op_3 }
          , triangle_svg { x = x_b, y = y_b, angle = -90.0, opacity = op_3 }
          , triangle_svg { x = x_a, y = y_a, angle = 180.0, opacity = op_3 }
          ]
        )
    ]
