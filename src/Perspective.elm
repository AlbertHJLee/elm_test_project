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
        []
        []
    , viewObjects model
    ]


viewObjects model =
  let
    objects = ( getObjects )
  in
  div
    [ Attr.style "width" "700px"
    , Attr.style "height" "700px"
    , Attr.style "background-color" "#808080"
    , Attr.style "margin" "auto"
    ]
    [ Svg.svg
        [ width "600px"
        , height "500px"
        ]
        (
          ( List.concat ( List.map viewPolygonMasked objects ) ) ++
          [ Svg.circle
              [ cx "50"
              , cy "50"
              , r "40"
              , fill "red"
              ]
              []
          ] ++
          [ Svg.defs
              []
              [ Svg.mask [ id "myMask" ]
                  [ Svg.circle [ cx "50", cy "50", r "40", fill "white" ] []
                  ]
              ]
          , Svg.rect
              [ x "60", y "4", width "60", height "56", fill "blue", mask "url(#myMask)" ]
              []
          ]
        )
    ]

-- ( text ( String.concat test_text ) )


getTextFromVec vec4 =
  let
    r = V4.toRecord vec4
  in
  ( String.fromFloat r.x ) ++ "," ++ ( String.fromFloat r.y ) ++ " "


viewPolygon poly color =
  let
    data = { x = 0, y = 0 }
    transform_text =
      "translate(" ++ (String.fromFloat data.x) ++ "," ++ (String.fromFloat data.y) ++ ")"
    points_text =
      String.concat ( List.map getTextFromVec poly )
  in
  Svg.polygon
    [ points points_text  -- "0,0 80,60 0,60 "  --
    , transform transform_text
    , fill color
    , fillOpacity "1.0"
    , strokeWidth "0"
    ]
    []


viewPolygonMasked : PolygonM -> List (Svg.Svg msg)
viewPolygonMasked polygon =
  let
    points_text =
      String.concat ( List.map getTextFromVec polygon.poly )
    mask_placeholder =
      case polygon.mask of
        Nothing ->
          []
        Just mask ->
          [ Svg.defs []
              [ Svg.mask [ id "mask1" ]
                  [ ( viewPolygon polygon.poly "white" )
                  , ( viewPolygon mask "black" )
                  ]
              ]
          ]
  in
  mask_placeholder ++
  [ Svg.polygon
      [ points points_text
      , fill "#c0c0ff"
      , fillOpacity "1.0"
      , strokeWidth "0"
      , mask "url(#mask1)"
      ]
      []
  ]


type alias Polygon =
  List V4.Vec4

type alias PolygonM =
  { poly : List V4.Vec4
  , mask : Maybe ( List V4.Vec4 )
  }


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


makePolygon : List ( Vector ) -> PolygonM
makePolygon vectorlist =
  { poly = List.map ( \v -> V4.vec4 v.x v.y v.z 1.0 ) vectorlist
  , mask = Nothing
  }

makePolygonMasked : List ( Vector ) -> List ( Vector ) -> PolygonM
makePolygonMasked vectorlist masklist =
  { poly = List.map ( \v -> V4.vec4 v.x v.y v.z 1.0 ) vectorlist
  , mask = Just ( List.map ( \v -> V4.vec4 v.x v.y v.z 1.0 ) masklist )
  }


getObjects : List ( PolygonM )
getObjects =
  let
    d = 20
    d1 = 400
    d2 = 200
    x1 = 100
    y1 = 100
  in
  [ makePolygon [ ( vector 60 4 0), ( vector 60 60 0 ), ( vector 120 60 0 ), ( vector 120 4 0 ) ]
  , makePolygon [ ( vector 4 4 0), ( vector 4 (4+d) 0 ), ( vector (4+d) (4+d) 0 ), ( vector (4+d) 4 0 ) ]
  , makePolygon [ ( vector 0 0 0), ( vector 0 d 0 ), ( vector d d 0 ), ( vector d 0 0 ) ]
  , makePolygonMasked
      [ ( vector 0 0 0), ( vector 0 d1 0 ), ( vector d1 d1 0 ), ( vector d1 0 0 ) ]
      [ ( vector x1 y1 0), ( vector (x1+d2) y1 0 ), ( vector (x1+d2) (y1+d2) 0 ), ( vector x1 (y1+d2) 0 ) ]
  ]
