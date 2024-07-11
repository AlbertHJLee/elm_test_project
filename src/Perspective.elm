--
-- Interactive app demonstrating perspective
--

module Perspective exposing (..)

import Browser
import Browser.Events as E
import Html exposing (Html, div, text)
import Html.Events exposing (onClick, onInput)
import Html.Attributes as Attr
import Json.Decode as D
import Time
import Task
import Array exposing (Array)

import Svg
import Svg.Attributes exposing (..)
import Ease

import Math.Vector4 as V4
import Math.Matrix4 as M4




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
  , width : Float
  , height : Float
  }


type ProjectionMode
  = Iso
  | OnePoint
  | TwoPoint
  | ThreePoint


type Msg
  = DoNothing
  | KeyChanged Bool String
  | TimeDelta Float
  | Resized Float Float
  | VisibilityChanged E.Visibility


type alias Keys =
  { up : Bool
  , left : Bool
  , down : Bool
  , z : Bool
  , x : Bool
  }


default_model : Model
default_model =
  { mode = Iso
  , eye = V4.vec4 0 0 0 0
  , width = 600
  , height = 600
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
  Sub.batch
    [ E.onResize (\w h -> Resized (toFloat w) (toFloat h))
    , E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
    , E.onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
    , E.onAnimationFrameDelta TimeDelta
    , E.onVisibilityChange VisibilityChanged
    ]




-- VIEW


view : Model -> Html Msg
view model =
  div
    -- [ Attr.style "position" "absolute"
    -- , Attr.style "left" "0"
    -- , Attr.style "top" "0"
    -- , Attr.style "width" (String.fromFloat model.width ++ "px")
    -- , Attr.style "height" (String.fromFloat model.height ++ "px")
    -- ]
    [ Attr.style "width" "device-width"
    , Attr.style "height" "100vh"
    , Attr.style "display" "flex"
    , Attr.style "flex-direction" "column"
    , Attr.style "justify-content" "center"
    , Attr.style "align-items" "center"
    , Attr.style "background-color" "#f0f0f0"
    ]
    [ div
        -- [ Attr.style "width" "200px"
        -- , Attr.style "height" "20px"
        -- , Attr.style "background-color" "#444444"
        -- , Attr.style "margin" "auto"
        -- ]
        []
        []
    , viewObjects model
    ]


viewObjects model =
  let
    d = 20
    objects =
      [ makePolygon [ ( vector 0 0 0), ( vector 0 d 0 ), ( vector d d 0 ), ( vector d 0 0 ) ]
      ]
    -- objects = ( getObjects )
    test_text =
      List.map ( \o -> String.concat ( List.map getTextFromVec o ) ) objects
  in
  div
    [ Attr.style "width" "700px"
    , Attr.style "height" "700px"
    , Attr.style "background-color" "#808080"
    , Attr.style "margin" "auto"
    ]
    [ ( text ( String.concat test_text ) )
    , Svg.svg
        [ width "600px"
        , height "500px"
        ]
        (
          ( List.map viewPolygon objects ) ++
          [ Svg.circle
              [ cx "50"
              , cy "50"
              , r "40"
              , fill "red"
              ]
              []
          ] ++
          ( List.map viewText objects )
        )
    ]


getTextFromVec vec4 =
  let
    vecrec = ( V4.toRecord vec4 )
    x = vecrec.x
    y = vecrec.y
  in
  ( String.fromFloat x ) ++ "," ++ ( String.fromFloat y ) ++ " "


viewPolygon poly =
  let
    data = { x = 100, y = 100 }
    transform_text =
      "translate(" ++ (String.fromFloat data.x) ++ "," ++ (String.fromFloat data.y) ++ ")"
    points_text =
      String.concat ( List.map getTextFromVec poly )
  in
  Svg.polygon
    [ points points_text  -- "0,0 80,60 0,60 "  --
    , transform transform_text
    , fill "#c0c0ff"
    , fillOpacity "1.0"
    , strokeWidth "0"
    ]
    [ text ( "test text" ++ points_text ) ]


viewText poly =
  let
    points_text =
      String.concat ( List.map getTextFromVec poly )
  in
  Svg.text_
    []
    [ text ( "viewtext test" ++ points_text ) ]


type alias Polygon =
  List V4.Vec4


type alias Vector =
  { x : Float
  , y : Float
  , z : Float
  }
vector : Float -> Float -> Float -> Vector
vector vx vy vz =
  { x = vx
  , y = vy
  , z = vz
  }


makePolygon : List ( Vector ) -> Polygon
makePolygon vectorlist =
  List.map ( \v -> V4.vec4 v.x v.y v.z 1.0 ) vectorlist


getObjects : List ( Polygon )
getObjects =
  let
    d = 20
  in
  [ makePolygon [ ( vector 0 0 0), ( vector 0 d 0 ), ( vector d d 0 ), ( vector d 0 0 ) ]
  , makePolygon [ ( vector 4 4 0), ( vector 4 (4+d) 0 ), ( vector (4+d) (4+d) 0 ), ( vector (4+d) 4 0 ) ]
  ]
