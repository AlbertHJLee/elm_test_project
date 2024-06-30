module Magnet exposing (..)

-- Draw a cicle around the mouse. Change its color by pressing down.
--
-- Adapted from https://elm-lang.org/examples/mouse
--
-- Learn more about the playground here:
--   https://package.elm-lang.org/packages/evancz/elm-playground/latest/
--

import Playground exposing (..)
import Html exposing (Html, button, div, text)


main =
  game view update ()




-- HELPER FUNCTIONS


displacement computer memory originx originy =

  let

    -- Calculate distance between mouse and object's origin
    mousex = computer.mouse.x
    mousey = computer.mouse.y
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

  in
  { newx = originx - ((deltax / r) * displaced_final)
  , newy = originy - ((deltay / r) * displaced_final)
  , distance = r
  , displaced = displaced
  }


column_of_locations computer memory y_list x =
  List.map (displacement computer memory x) y_list


grid_of_locations computer memory x_list y_list =
  List.map (column_of_locations computer memory y_list) x_list
    |> List.concat




-- VIEW


plot_circle computer new_location =
  let
    x = new_location.displaced
    -- xscaled = if (x <= 1) then x else (1 - 1/(x+1))
    xscaled = 1 - 8/(x+8)
    red0 = 173
    redf = (255-red0) * xscaled + red0
    ccolor = rgb redf 80 168
    -- ccolor = rgb redf 127 168
  in
  -- circle lightPurple 30
  circle ccolor 30
    |> moveX new_location.newx
    |> moveY new_location.newy
    |> fade (if computer.mouse.down then 0.5 else 1)


view computer memory =

  let
    location_grid = grid_of_locations computer memory
      [-400,-300,-200,-100,   0, 100, 200, 300, 400]
      [-200,-100,   0, 100, 200]

  in
  [words black (String.fromFloat 100.0)
      |> move 0 -300
  ] ++
  [ circle red 10
      |> moveX computer.mouse.x
      |> moveY computer.mouse.y
      |> fade (if computer.mouse.down then 0.2 else 1)
  ] ++
  (List.map (plot_circle computer) location_grid)

--    ,
--    div []
--      [ text (String.fromFloat new_location.newx)
--      , text (String.fromFloat new_location.newy)
--      ]


update computer memory =
  memory
