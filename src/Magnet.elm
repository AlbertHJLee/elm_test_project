module Magnet exposing (..)

-- Draw a circle around the mouse. Change its color by pressing down.
--
-- Adapted from https://elm-lang.org/examples/mouse
--
-- Learn more about the playground here:
--   https://package.elm-lang.org/packages/evancz/elm-playground/latest/
--

import Playground exposing (..)
import Html exposing (Html, button, div, text)




main =
  -- game view update ()
  game view update (init_circles)


init_circles = grid_of_circles
  [-400,-300,-200,-100,   0, 100, 200, 300, 400]
  [-200,-100,   0, 100, 200]

grid_of_circles x_list y_list =
  List.map (column_of_circles y_list) x_list
    |> List.concat

column_of_circles y_list x =
  List.map (init_circle x) y_list

init_circle x y =
  let
    results = displacement 0 -500 x y
  in
  { x = x
  , y = y
  , flagged = False
  , newx = results.newx
  , newy = results.newy
  , distance = results.distance
  , displaced = results.displaced
  , angle = results.angle
  }

type alias Model =
  List
  { x : Number
  , y : Number
  , flagged : Bool
  , newx : Number
  , newy : Number
  , distance : Float
  , displaced : Float
  , angle : Float
  }




-- HELPER FUNCTIONS


displacement mousex mousey originx originy =

  let

    -- Calculate distance between mouse and object's origin
    -- mousex = computer.mouse.x
    -- mousey = computer.mouse.y
    deltax = mousex - originx
    deltay = mousey - originy
    r0 = sqrt(deltax^2 + deltay^2)

    min_r = 0.1
    r = if (r0 < min_r) then min_r else r0

    -- Calculate distance to displace object from its origin
    --
    -- The force from the mouse is proportional to inverse of distance r squared,
    -- like gravity. The force holding the object to the origin is proportional
    -- to its displacement d, like a mechanical spring.
    --
    -- In other words, g * r^(-2) = k * d
    -- or: d = r^(-2) * g / k

    spring_constant = 2.5
    displaced = (r^(-2)) * 600000.0 / spring_constant

    max_displaced = 40
    displaced_final = if (displaced < max_displaced) then displaced else max_displaced

    angle = atan (deltay/deltax)

  in
  { newx = originx - ((deltax / r) * displaced_final)
  , newy = originy - ((deltay / r) * displaced_final)
  , distance = r
  , displaced = displaced
  , angle = angle
  }


-- column_of_positions computer memory y_list x =
--   List.map (displacement computer memory x) y_list


-- grid_of_positions computer memory x_list y_list =
--   List.map (column_of_positions computer memory y_list) x_list
--     |> List.concat




-- VIEW


plot_circle computer position_data =

  let
    x = position_data.displaced
    xscaled = 1 - 8/(x+8)

    red0 = 133
    red1 = 255
    redf = (red1 - red0) * xscaled + red0
    green0 = 127
    green1 = 139
    greenf = (green1 - green0) * xscaled + green0
    blue0 = 168
    blue1 = 160
    bluef = (blue1 - blue0) * xscaled + blue0

    ccolor = rgb redf 80 bluef
    -- ccolor = rgb 173 127 168 -- 255 130 5

    squashfactor = 0.7 + 24/(x+80)

  in
  oval ccolor (60*squashfactor) 60
    |> moveX position_data.newx
    |> moveY position_data.newy
    |> rotate (position_data.angle / pi * 180)
    -- |> fade (if computer.mouse.down then 0.5 else 1)


view computer circles =

  let
    -- circle_grid = grid_of_positions computer memory
    --   [-400,-300,-200,-100,   0, 100, 200, 300, 400]
    --   [-200,-100,   0, 100, 200]

    -- first_circle = displacement computer memory 0 0
    first_circle = displacement computer.mouse.x computer.mouse.y 0 0

  in
  [words black ("Angle: " ++ (String.fromFloat first_circle.angle))
      |> move 0 -300
  ]
  ++
  [ circle (rgb 252 233 79) (if computer.mouse.down then 10 else 26)
      |> moveX computer.mouse.x
      |> moveY computer.mouse.y
      |> fade (if computer.mouse.down then 1 else 0.6)
  ]
  ++
  (List.map (plot_circle computer) circles)




-- UPDATE


update_circle computer circle =
  let
    results = displacement computer.mouse.x computer.mouse.y circle.x circle.y
  in
  { circle | flagged = (if results.distance < 5 then True else circle.flagged)
  , newx = results.newx
  , newy = results.newy
  , distance = results.distance
  , displaced = results.displaced
  , angle =  results.angle
  }


update: Computer -> Model -> Model
update computer circles =
  List.map (update_circle computer) circles
