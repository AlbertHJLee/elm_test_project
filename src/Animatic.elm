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

import Svg exposing (svg, circle, rect, line)
import Svg.Attributes exposing (..)




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
  , Cmd.none
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
        ( { model
            | index = model.index + 1
          }
        , Task.perform SetTime Time.now
        )

    Previous ->
      if model.index == 0 then
        ( model
        , Cmd.none
        )
      else
        ( { model
            | index = model.index - 1
          }
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
      ( { model | current_time = newTime }
      , Cmd.none
      )

    Reset ->
      ( model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  -- Time.every 1000 Tick
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
            , Attr.style "height" "160px"
            , Attr.style "text-align" "center"
            , Attr.style "background-color" "green"
            , Attr.style "margin" "10px auto"
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
        1 -> (scene_one model)
        2 -> (scene_two model)
        _ -> div [] []
    ]


-- NOTE: This may be an unstable way to get the styles for a Html.div function
-- May want to refactor
mainStyle : List (Html.Attribute Msg)
mainStyle =
  [ Attr.style "width" "400px"
  , Attr.style "height" "400px"
  , Attr.style "background-color" "yellow"
  -- , Attr.style "justify-content" "center"
  -- , Attr.style "align-items" "center"
  -- , Attr.style "display" "flex"
  -- , Attr.style "text-align" "center"
  ]



-- SCENES

scene_zero_to_one : Model -> Html Msg
scene_zero_to_one model =
  div
    []
    []

scene_one : Model -> Html Msg
scene_one model =
  let
    diff = (Time.posixToMillis model.current_time) - (Time.posixToMillis model.click_time)
    angle = (toFloat diff) * 0.001 * 30.0
    x0 = 100
    y0 = 15
    w0 = 40
    h0 = 70
    rx = x0 + (0.5 * w0)
    ry = y0 + (0.5 * h0)
  in
  div
    [ ]
    [ svg
        [ viewBox "0 0 400 400"
        , width "400"
        , height "400"
        ]
        [ rect
            [ x (String.fromInt x0)
            , y (String.fromInt y0)
            , width (String.fromInt w0)
            , height (String.fromInt h0)
            , fill "blue"
            , stroke "black"
            , strokeWidth "2"
            , transform
                ( "rotate( " ++
                  (String.fromFloat angle) ++ " " ++
                  (String.fromFloat rx) ++ " " ++
                  (String.fromFloat ry) ++ " )"
                )
            ]
            []
        ]
    ]

scene_two model =
  div [] []
