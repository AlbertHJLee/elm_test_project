module Main exposing (..)

-- Draw a cicle around the mouse. Change its color by pressing down.
--
-- Learn more about the playground here:
--   https://package.elm-lang.org/packages/evancz/elm-playground/latest/
--

import Browser
import Html exposing (Html, button, div, text, p)
import Html.Events exposing (onClick)
import Html.Attributes exposing (width, height, style)


import Playground exposing (..)
import Browser.Events as E




-- MAIN

-- main =
--   game viewC updateC ()

-- main_old =
--   Browser.element
--     { init = init
--     , view = view
--     , update = \msg model -> (update msg model, Cmd.none)
--     , subscriptions = subscriptions
--     }

main =
  Browser.sandbox { init = init, update = update, view = view }




-- MODEL

type alias Model = Int

init : Model
init =
  0

type alias HtmlModel =
  { keys : Keys
  , width : Float
  , height : Float
  , person : Person
  , texture : Float
  }

type alias Keys =
  { up : Bool
  , left : Bool
  , down : Bool
  , right : Bool
  , space : Bool
  }

-- type alias Person =
--   { position : Vec3
--   , velocity : Vec3
--   }

type alias Person =
  { position : Float
  , velocity : Float
  }




-- UPDATE

updateC computer memory =
  memory

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

subscriptions : Model -> Sub MsgB
subscriptions model =
  Sub.batch
    [ E.onResize (\w h -> Resized (toFloat w) (toFloat h))
    ]

type MsgB
  = KeyChanged Bool String
  | TimeDelta Float
  | Resized Float Float
  | VisibilityChanged E.Visibility




-- VIEW

viewH : Model -> HtmlModel -> Html Msg
viewH model htmlmodel =
  div
    [ style "position" "absolute"
    , style "left" "0"
    , style "top" "0"
    , style "width" (String.fromFloat htmlmodel.width ++ "px")
    , style "height" (String.fromFloat htmlmodel.height ++ "px")
    ]
    [
    testdiv
    ]

view : Model -> Html Msg
view model =
  div
    [ style "position" "absolute"
    , style "left" "0"
    , style "top" "0"
    , style "width" (String.fromFloat 300 ++ "px")
    , style "height" (String.fromFloat 300 ++ "px")
    ]
    [
    div [] [(buttondiv model)],
    testdiv
    ]

testdiv: Html Msg
testdiv =
  div
    []
    [ p [] [text "Hello world." ] ]

viewC computer memory =
  [ circle lightPurple 30
      |> moveX computer.mouse.x
      |> moveY computer.mouse.y
      |> fade (if computer.mouse.down then 0.2 else 1)
  ]

buttondiv: Model -> Html Msg
buttondiv model =
  div
    [ style "position" "absolute"
    , style "font-family" "monospace"
    , style "text-align" "center"
    , style "left" "20px"
    , style "right" "20px"
    , style "top" "20px"
    ]
    [
    button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]



-- OLD

-- update computer memory =
--   memory
