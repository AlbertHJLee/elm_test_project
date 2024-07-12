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




--
-- MAIN
--


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }




--
-- MODEL
--


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
  , eye = V4.vec4 12 12 -40 0
  , width = 600
  , height = 600
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( default_model , Cmd.none)




--
-- UPDATE
--


update : Msg -> Model -> (Model, Cmd Msg)
update msg model  =
  ( model, Cmd.none )


transformVectorIso : V4.Vec4 -> V4.Vec4 -> V4.Vec4
transformVectorIso eye vector_in =
  let
    e = V4.toRecord eye
    v = V4.toRecord vector_in
    v_new =
      { v | x = v.x + e.x * v.z
          , y = v.y + e.y * v.z
      }
  in
  V4.fromRecord v_new


transformPolygonIso : V4.Vec4 -> PolygonM -> PolygonM
transformPolygonIso eye polygon =
  let
    poly_vectors = polygon.poly
    new_p_vectors = List.map ( \v -> transformVectorIso eye v ) poly_vectors
    new_m_vectors =
      case polygon.mask of
        Nothing ->
          Nothing
        Just mask ->
          Just ( List.map ( \v -> transformVectorIso eye v ) mask )
    -- mask_vectors = polygon.mask
    -- new_m_vectors = List.map ( \v -> transformVectorIso eye v ) mask_vectors
  in
  { polygon
      | poly = new_p_vectors
      , mask = new_m_vectors
  }


transformIso : Model -> List PolygonM -> List PolygonM
transformIso model polygons =
  let
    eye = model.eye
    new_polygons = List.map ( \p -> transformPolygonIso eye p ) polygons
  in
  new_polygons




--
-- SUBSCRIPTIONS
--


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ E.onResize (\w h -> Resized (toFloat w) (toFloat h))
    , E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
    , E.onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
    , E.onAnimationFrameDelta TimeDelta
    , E.onVisibilityChange VisibilityChanged
    ]




--
-- VIEW
--


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


viewParams =
  { window_width = 700
  , window_height = 700
  , bgcolor = "#186BB4"
  }


toPx length =
  ( String.fromFloat length ) ++ "px"


viewObjects model =
  let
    objects = ( getObjects )
    -- Transform and Project polygons
    polygonsTransformed = transformIso model objects
    -- Prepare a unique integer ID for each masked polygon
    polygonsWithIDs = addMaskIds polygonsTransformed
  in
  div
    [ Attr.style "width" ( toPx viewParams.window_width )
    , Attr.style "height" ( toPx viewParams.window_height )
    , Attr.style "background-color" "#808080"
    , Attr.style "margin" "auto"
    ]
    [ Svg.svg
        [ width ( toPx viewParams.window_width )
        , height ( toPx viewParams.window_height )
        ]
        (
          [ viewBackground ] ++
          polygonsWithIDs ++
          []  -- Placeholder for future content
        )
    ]


viewBackground =
  Svg.rect
    [ width ( toPx viewParams.window_width )
    , height ( toPx viewParams.window_height )
    , fill viewParams.bgcolor
    ]
    []


addMaskIds polygons =
  let
    n_polygons = List.length polygons
    ids = List.range 1 n_polygons
    id_texts = List.map (\id -> "mask" ++ ( String.fromInt id ) ) ids
    polygonsWithIDs = List.concat ( List.map2 viewPolygonMasked polygons id_texts )
  in
  polygonsWithIDs


getTextFromVec vec4 =
  let
    r = V4.toRecord vec4
  in
  ( String.fromFloat r.x ) ++ "," ++ ( String.fromFloat r.y ) ++ " "


viewPolygon poly color mask_id =
  let
    points_text =
      String.concat ( List.map getTextFromVec poly )
    mask_attr =
      case mask_id of
        "" ->
          []
        _ ->
          [ mask ( "url(#" ++ mask_id ++ ")" ) ]
  in
  Svg.polygon
    ( [ points points_text
      , fill color
      , fillOpacity "1.0"
      , strokeWidth "0"
      ]
      ++ mask_attr
    )
    []


viewPolygonMasked : PolygonM -> String -> List (Svg.Svg msg)
viewPolygonMasked polygon mask_id =
  let
    points_text =
      String.concat ( List.map getTextFromVec polygon.poly )
    mask_placeholder =
      case polygon.mask of
        Nothing ->
          []
        Just mask ->
          [ Svg.defs []
              [ Svg.mask [ id mask_id ]
                  [ ( viewPolygon polygon.poly "white" "" )
                  , ( viewPolygon mask "black" "" )
                  ]
              ]
          ]
  in
  mask_placeholder ++
  [ ( viewPolygon polygon.poly "#c0c0ff" mask_id ) ]




--
-- POLYGONS
--


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


makeFrameStack =
  let
    cx = viewParams.window_width / 2
    cy = viewParams.window_height / 2
    s1 = 400
    s2 = 200
    x1 = cx - s1 / 2
    y1 = cy - s1 / 2
    z1 = 0
    n = 4
    z_values = List.range z1 (z1 + n - 1)
  in
  List.map (\z -> makePolyFrame x1 y1 (toFloat z) s1 s2 ) z_values


makePolyFrame x y z outside inside =
  let
    frameWidth = (outside - inside) / 2
    xi = x + frameWidth
    yi = y + frameWidth
  in
  makePolygonMasked
    [ ( vector x y z ), ( vector (x+outside) y z ), ( vector (x+outside) (y+outside) z ), ( vector x (y+outside) z ) ]
    [ ( vector xi yi z ), ( vector (xi+inside) yi z ), ( vector (xi+inside) (yi+inside) z ), ( vector xi (yi+inside) z ) ]


getObjects : List ( PolygonM )
getObjects =
  let
    cx = viewParams.window_width / 2
    cy = viewParams.window_height / 2
    d = 20
    s1 = 400
    s2 = 200
    x1 = cx - s1 / 2
    y1 = cy - s1 / 2
  in
  [ makePolygon [ ( vector 4 4 0), ( vector 4 (4+d) 0 ), ( vector (4+d) (4+d) 0 ), ( vector (4+d) 4 0 ) ]
  , makePolygon [ ( vector 0 0 0), ( vector 0 d 0 ), ( vector d d 0 ), ( vector d 0 0 ) ]
  , makePolyFrame x1 y1 0 s1 s2
  , makePolyFrame (x1+d) (y1+d) 0 s1 s2
  ] ++
  makeFrameStack
