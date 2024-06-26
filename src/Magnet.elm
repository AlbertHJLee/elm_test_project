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


displacement computer memory = 
  let
    mousex = computer.mouse.x
    mousey = computer.mouse.y
    centerx = 300
    centery = 300
    deltax = mousex - centerx
    deltay = mousey - centery
  in
  { newx = centerx + (deltax*0.5)
  , newy = centery + (deltay*0.5)
  }


displacement2 computer memory = 
  let
    mousex = computer.mouse.x
    mousey = computer.mouse.y
    centerx = 300
    centery = 300
    deltax = mousex - centerx
    deltay = mousey - centery
    r = sqrt(deltax^2 + deltay^2)
    -- force = (deltax^2 + deltay^2)^(-1.5)
    spring_constant = 0.5
    displaced = (r^(-2)) * 1000000.0 / spring_constant
    max_displaced = 100
    displaced_final = if (displaced < max_displaced) then displaced else max_displaced
  in
  { newx = centerx - ((deltax / r) * displaced_final)
  , newy = centery - ((deltay / r) * displaced_final)
  }


view computer memory =
  let
    new_location = (displacement2 computer memory)
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
      |> moveX new_location.newx
      |> moveY new_location.newy
      |> fade (if computer.mouse.down then 0.5 else 1)
--    ,
--    div [] 
--      [ text (String.fromFloat new_location.newx)
--      , text (String.fromFloat new_location.newy)
--      ]
  ]


update computer memory =
  memory
