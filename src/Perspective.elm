--
-- Interactive app demonstrating perspective
--

module Perspective exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Events exposing (onClick, onInput)
import Html.Attributes as Attr
import Time
import Task

import Svg exposing (svg, circle, rect, line, polygon, text_)
import Svg.Attributes exposing (..)
import Ease

import Math.Vector4 as V4




-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }




-- MODEL


type alias Model =
  { mode : ProjectionMode
  , eye : V4.Vec4
  }


type ProjectionMode
  = Iso
  | OnePoint
  | TwoPoint
  | ThreePoint


type Msg
  = DoNothing


default_model : Model
default_model =
  { mode = Iso
  , eye = V4.vec4 0 0 0 0
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( default_model , Cmd.none)




-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model  =
  ( model, Cmd.none )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none




-- VIEW


view : Model -> Html Msg
view model =
  div
    []
    []
