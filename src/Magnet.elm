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
  
  
type alias Model = 
  { 
  }


displacement computer memory centerx centery = 
  let
    mousex = computer.mouse.x
    mousey = computer.mouse.y
    deltax = mousex - centerx
    deltay = mousey - centery
    r = sqrt(deltax^2 + deltay^2)
    spring_constant = 2.5
    displaced = (r^(-2)) * 1000000.0 / spring_constant
    max_displaced = 100
    displaced_final = if (displaced < max_displaced) then displaced else max_displaced
  in
  { newx = centerx - ((deltax / r) * displaced_final)
  , newy = centery - ((deltay / r) * displaced_final)
  }


plot_circle computer new_location =
  circle lightPurple 30
    |> moveX new_location.newx
    |> moveY new_location.newy
    |> fade (if computer.mouse.down then 0.5 else 1)
  

view computer memory =
  let
    new_location1 = (displacement computer memory 300 300)
    new_location2 = (displacement computer memory 200 300)
    new_location3 = (displacement computer memory 300 200)
    new_location4 = (displacement computer memory 200 200)
    location_list = List.map (displacement computer memory 100) 
      [100, 200, 300]
  in
  [ circle lightPurple 30
      |> moveX computer.mouse.x
      |> moveY computer.mouse.y
      |> fade (if computer.mouse.down then 0.2 else 1)
    ,
    circle lightPurple 30
      |> moveX (computer.mouse.x + 30)
      |> moveY (computer.mouse.y + 30)
    ,
    circle lightPurple 30
      |> moveX 300
      |> moveY 300
      |> fade (if computer.mouse.down then 0.2 else 1)
    ,
    circle lightPurple 30
      |> moveX new_location1.newx
      |> moveY new_location1.newy
      |> fade (if computer.mouse.down then 0.5 else 1)
    ,
    circle lightPurple 30
      |> moveX new_location2.newx
      |> moveY new_location2.newy
      |> fade (if computer.mouse.down then 0.5 else 1)
    ,
    circle lightPurple 30
      |> moveX new_location3.newx
      |> moveY new_location3.newy
      |> fade (if computer.mouse.down then 0.5 else 1)
    ,
    circle lightPurple 30
      |> moveX new_location4.newx
      |> moveY new_location4.newy
      |> fade (if computer.mouse.down then 0.5 else 1)
  ] ++ (List.map (plot_circle computer) location_list)

--    ,
--    div [] 
--      [ text (String.fromFloat new_location.newx)
--      , text (String.fromFloat new_location.newy)
--      ]


update computer memory =
  memory
