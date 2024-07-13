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
import Task

import Svg
import Svg.Attributes exposing (..)
import Ease

import Math.Vector4 as V4
import Math.Matrix4 as M4

import Hex




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
  , keys : Keys
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
  | ModeChanged String


type alias Keys =
  { up : Bool
  , left : Bool
  , down : Bool
  , right : Bool
  , space : Bool
  , z : Bool
  , x : Bool
  , other : Bool
  }


hyperParams =
  { maxval = 12
  , focal_length = viewParams.window_width
  }


default_model : Model
default_model =
  let
    hv = hyperParams.maxval / 2
  in
  { mode = Iso
  , eye = V4.vec4 hv hv ( -4 * hv ) 0
  , width = 600
  , height = 600
  , keys = Keys False False False False False False False False
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( default_model , Cmd.none)




--
-- UPDATE
--


update : Msg -> Model -> (Model, Cmd Msg)
update msg model  =
  case msg of

    DoNothing ->
      ( model, Cmd.none )

    KeyChanged isDown key ->
      ( { model | keys = updateKeys isDown key model.keys }
      , if ( isNumber key )
        then Task.perform ModeChanged ( Task.succeed key )
        else Cmd.none
      )

    TimeDelta dt ->
      ( ( updateEye model dt )
      , Cmd.none
      )

    Resized x y ->
      ( model, Cmd.none )

    VisibilityChanged v ->
      ( model, Cmd.none )

    ModeChanged key ->
      ( ( updateMode key model )
      , Cmd.none
      )


updateKeys : Bool -> String -> Keys -> Keys
updateKeys isDown key keys =
  case key of
    " "          -> { keys | space = isDown }
    "ArrowUp"    -> { keys | up    = isDown }
    "ArrowLeft"  -> { keys | left  = isDown }
    "ArrowDown"  -> { keys | down  = isDown }
    "ArrowRight" -> { keys | right = isDown }
    _            -> { keys | other = isDown }


anyDown keys =
  keys.space || keys.up || keys.left || keys.down || keys.right


isNumber : String -> Bool
isNumber s =
  case ( String.toInt s ) of
    Just n ->
      True
    Nothing ->
      False


updateMode : String -> Model -> Model
updateMode key model =
  case key of
    "1" ->
      { model | mode = OnePoint }
    "2" ->
      { model | mode = TwoPoint }
    "0" ->
      { model | mode = Iso }
    _ ->
      model


updateEye : Model -> Float -> Model
updateEye model dt =
  let
    e = model.eye
    k = model.keys
    fromKey isDown =
      if isDown then 1.0 else 0.0
    maxval = hyperParams.maxval
    v =
      if ( not ( anyDown k ) )
        then
          V4.vec4 ( maxval / 2 ) ( maxval / 2 ) ( -2 * maxval ) 0
      else if k.space
        then
          V4.vec4 0 0 -24 0
        else
          V4.vec4
            ( maxval * ( ( fromKey k.right ) - ( fromKey k.left ) ) )
            ( maxval * ( ( fromKey k.down ) - ( fromKey k.up ) ) )
            -24
            0
    d = V4.sub v e
    dsec = dt / 1000.0
    inverse_damping = 4.0
    e_new = V4.add e ( V4.scale ( dsec * inverse_damping ) d )
  in
    { model | eye = e_new }


{-|
In isomorphic transformations, the eye is technically infinitely far away,
but here we will use a finite vector to determine the direction of the eye.
--
Also, the transform functions are called in ( view ) but we list them here because
they are directly dependent on model.eye, which is updated by ( update ).
-}
transformVectorIso : Model -> V4.Vec4 -> V4.Vec4
transformVectorIso model vector_in =
  let
    eye = model.eye
    e = V4.toRecord eye
    v = V4.toRecord vector_in
    v_new =
      { v | x = v.x - e.x / e.z * v.z
          , y = v.y - e.y / e.z * v.z
      }
  in
  V4.fromRecord v_new


transformVectorOne : Model -> V4.Vec4 -> V4.Vec4
transformVectorOne model vector_in =
  let
    eye = model.eye
    relative_position = V4.sub vector_in eye
    r = V4.toRecord relative_position
    e = V4.toRecord eye
    f = hyperParams.focal_length

    -- First transform r vector onto imaging plane
    r_new = toImagePlaneOne f r

    -- Then recenter image based on eye's position relative to origin
    e_new = toImagePlaneOne f e
  in
  -- Real image is r_new - e_new, but since this is flipped
  -- we unflip when drawing the svg by using e_new - r_new instead
  V4.sub ( V4.fromRecord e_new ) ( V4.fromRecord r_new )


-- toImagePlaneOne : Float -> Vector -> Vector
toImagePlaneOne f vector_record =
  let
    r = vector_record
  in
  { r
      | x = -r.x / r.z * f
      , y = -r.y / r.z * f
      , z = -f
    }


transformVector : Model -> V4.Vec4 -> V4.Vec4
transformVector model vector_in =
  case model.mode of
    Iso ->
      transformVectorIso model vector_in
    OnePoint ->
      transformVectorOne model vector_in
    _ ->
      transformVectorOne model vector_in


transformPolygon : Model -> PolygonM -> PolygonM
transformPolygon model polygon =
  let
    poly_vectors = polygon.poly
    new_p_vectors = List.map ( \v -> transformVector model v ) poly_vectors
    new_m_vectors =
      case polygon.mask of
        Nothing ->
          Nothing
        Just mask ->
          Just ( List.map ( \v -> transformVector model v ) mask )
  in
  { polygon
      | poly = new_p_vectors
      , mask = new_m_vectors
  }


transformObjects : Model -> List PolygonM -> List PolygonM
transformObjects model polygons =
  let
    new_polygons = List.map ( \p -> transformPolygon model p ) polygons
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
  , fade_factor = 0.999
  }


toPx length =
  ( String.fromFloat length ) ++ "px"


viewObjects model =
  let
    objects = ( getObjects )

    -- Get Colors based on positions
    colors = List.map colorByPosition objects

    -- Transform and Project polygons
    polygonsTransformed = transformObjects model objects

    -- Prepare a unique integer ID for each masked polygon and add colors
    polygonsWithIDs = addMaskIds colors polygonsTransformed
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


addMaskIds colors polygons =
  let
    n_polygons = List.length polygons
    ids = List.range 1 n_polygons
    id_texts = List.map (\id -> "mask" ++ ( String.fromInt id ) ) ids
    -- colors = List.map colorByPosition polygons
    polygonsWithIDs = List.concat ( List.map3 viewPolygonMasked polygons colors id_texts )
  in
  polygonsWithIDs


getTextFromVec : V4.Vec4 -> String
getTextFromVec vec4 =
  let
    r = V4.toRecord vec4
  in
  ( String.fromFloat r.x ) ++ "," ++ ( String.fromFloat r.y ) ++ " "


reCenter : Float -> Float -> V4.Vec4 -> V4.Vec4
reCenter cx cy vector_in =
  let
    v = V4.toRecord vector_in
  in
  { v | x = ( v.x + cx ), y = ( v.y + cy ) }
    |> V4.fromRecord


absoluteCoordinates : Polygon -> Polygon
absoluteCoordinates polygon =
  let
    cx = viewParams.window_width / 2
    cy = viewParams.window_height / 2
  in
  List.map
    ( reCenter cx cy )
    polygon


viewPolygon : Polygon -> String -> String -> Svg.Svg msg
viewPolygon poly color mask_id =
  let
    points_text =
      String.concat ( List.map getTextFromVec ( absoluteCoordinates poly ) )
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


viewPolygonMasked : PolygonM -> String -> String -> List (Svg.Svg msg)
viewPolygonMasked polygon color mask_id =
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
  [ ( viewPolygon polygon.poly color mask_id ) ]


colorByPosition : PolygonM -> String
colorByPosition polygon =
  let
    -- sky color: "#186BB4"
    -- polygon color: "#f8f8f8"
    rdec = Hex.fromString "18"
    gdec = Hex.fromString "6b"
    bdec = Hex.fromString "b4"
    rInt =
      case rdec of
        Ok value ->
          value
        _ ->
          24
    gInt =
      case gdec of
        Ok value ->
          value
        _ ->
          107
    bInt =
      case bdec of
        Ok value ->
          value
        _ ->
          180
    poly = polygon.poly
    z =
      case ( List.head poly ) of
        Just v ->
          V4.getZ v
        _ ->
          0
    fade = ( viewParams.fade_factor ) ^ z
    brightness = 255  -- 248
    r = interpolate ( toFloat rInt ) brightness fade
    g = interpolate ( toFloat gInt ) brightness fade
    b = interpolate ( toFloat bInt ) brightness fade
  in
  "#" ++ ( Hex.toString (round r) ) ++ ( Hex.toString (round g) ) ++ ( Hex.toString (round b) )


interpolate : Float -> Float -> Float -> Float
interpolate x1 x2 theta =
  x1 + (x2 - x1) * theta




--
-- POLYGONS
--


type alias Polygon =
  List V4.Vec4


type alias PolygonM =
  { poly : Polygon
  , mask : Maybe Polygon
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
    s1 = 400
    s2 = 200
    x1 = -s1 / 2
    y1 = -s1 / 2
    z1 = 0
    n = 4
    z_spacing = 96
    z_i = List.range 1 n
    z_max = z1 + n
    z_values = List.map ( \z -> (z_max - z) * z_spacing ) z_i
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


reCenterVector : Float -> Float -> Vector -> Vector
reCenterVector cx cy v =
  { v | x = ( v.x + cx ), y = ( v.y + cy ) }


makeLogo =
  let
    cx = viewParams.window_width / 2
    cy = viewParams.window_height / 2
    d = 20
    vlist1 = [ ( vector 4 4 0), ( vector 4 (4+d) 0 ), ( vector (4+d) (4+d) 0 ), ( vector (4+d) 4 0 ) ]
    vlist2 = [ ( vector 26 4 0), ( vector 26 (4+d) 0 ), ( vector (26+d) (4+d) 0 ) ]
  in
  [ makePolygon ( List.map ( reCenterVector -cx -cy ) vlist1 )
  , makePolygon ( List.map ( reCenterVector -cx -cy ) vlist2 )
  ]


getObjects : List ( PolygonM )
getObjects =
  makeLogo ++
  makeFrameStack
