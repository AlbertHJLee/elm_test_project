module Main exposing (..)

--
-- Click through an animation of a geometry concept
--

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Time


-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Model =
  { index : Int
  , max_scenes : Int
  , click_time : Time.Posix
  }

init : Model
init =
  { index = 0
  , max_scenes = 4
  , click_time = (Time.millisToPosix 0)
  }



-- UPDATE

type Msg =
  Previous | Next

update msg model =
  case msg of

    Next ->
      if model.index == model.max_scenes then
        model
      ELSE
        { model | index = model.index + 1 }

    Previous ->
      if model.index == 0 then
          model
      ELSE
        { model | index = model.index - 1 }

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick



-- SCENES

scene_zero_to_one =
