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
  { maxval = 600
  , window_size = 700
  , frame_inside = 200
  }


vectorParams =
  let
    hv = hyperParams.maxval / 2
  in
  { default_eye = V4.vec4 hv hv ( -4 * hv ) 0
  , max_val = hv * 2
  , focal_length = ( hyperParams.frame_inside * 2 ) * 2.0  -- frame_outside * atan
  , inverse_damping = 4.0
  , default_atan = 2.0
  , z_spacing = 96
  }


viewParams =
  { window_width = hyperParams.window_size
  , window_height = hyperParams.window_size
  , menu_height = 300
  , bgcolor = "#186BB4"
  , fade_factor = 0.999
  , frame_outside = hyperParams.frame_inside * 2
  , frame_inside = hyperParams.frame_inside
  , cross_length = 20
  , stroke_halfwidth = 1
  , button_height = 40
  , button_margin = 4
  , button_stroke = 2
  }


font_attributes =
  [ Attr.style "font-family" "monospace"
  , Attr.style "user-select" "none"             -- Chrome, Edge, Firefox
  , Attr.style "-webkit-user-select" "none"     -- Safari
  , Attr.style "-webkit-touch-callout" "none"   -- iOS Safari
  , Attr.style "font-size" ( toPx 20 )
  ]


button_attributes color =
    [ Attr.style "width" ( toPx viewParams.button_height )
    , Attr.style "height" ( toPx viewParams.button_height )
    , Attr.style "margin" ( toPx viewParams.button_margin )
    , Attr.style "border" ( ( toPx viewParams.button_stroke ) ++ " solid " ++ color )
    , Attr.style "border-radius" "8px"
    , Attr.style "display" "flex"
    , Attr.style "align-items" "center"
    , Attr.style "justify-content" "center"
    , Attr.style "float" "left"
    , Attr.style "color" color
    ]


default_model : Model
default_model =
  { mode = Iso
  , eye = vectorParams.default_eye
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
    maxval = vectorParams.max_val
    d_atan = vectorParams.default_atan
    v =
      if ( not ( anyDown k ) )
        then
          vectorParams.default_eye
      else if k.space
        then
          V4.vec4 0 0 ( -d_atan * maxval ) 0
        else
          V4.vec4
            ( maxval * ( ( fromKey k.right ) - ( fromKey k.left ) ) )
            ( maxval * ( ( fromKey k.down ) - ( fromKey k.up ) ) )
            ( -d_atan * maxval )
            0
    d = V4.sub v e
    dsec = dt / 1000.0
    inverse_damping = vectorParams.inverse_damping
    e_new = V4.add e ( V4.scale ( dsec * inverse_damping ) d )
  in
    { model | eye = e_new }


{-|
In isometric transformations, the eye is technically infinitely far away,
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
    origin_relative = V4.negate eye
    r = V4.toRecord relative_position
    o = V4.toRecord origin_relative

    -- Get Z coordinate of focal plane
    f = V4.getZ vectorParams.default_eye

    -- First transform r vector onto imaging plane
    r_new = toImagePlaneOne f r

    -- Then recenter image based on the origin's position relative to the eye
    o_new = toImagePlaneOne f o
  in
  -- Real image is r_new - o_new, but since this is flipped
  -- we unflip when drawing the svg by using o_new - r_new instead
  V4.sub ( V4.fromRecord o_new ) ( V4.fromRecord r_new )


-- toImagePlaneOne : Float -> Vector -> Vector
toImagePlaneOne f vector_record =
  let
    v = vector_record
  in
  { v
      | x = v.x / v.z * f
      , y = v.y / v.z * f
      , z = f
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
        [ Attr.style "height" ( toPx viewParams.menu_height ) ]
        []
    , viewObjects model
    , div
        [ Attr.style "height" ( toPx viewParams.menu_height ) ]
        ( viewControls model )
    ]


toPx length =
  ( String.fromFloat length ) ++ "px"


viewControls model =
  let
    mode = model.mode
    mode_text =
      case mode of
        Iso ->
          "isometric"
        OnePoint ->
          "one-point"
        _ ->
          "one-point"
    number_button n color =
      div
        ( button_attributes color ++ font_attributes )
        [ text ( String.fromInt n ) ]
    space_width = 160
    space_left = 104
    space_button color =
      div
        (
          button_attributes color ++
          font_attributes         ++
          [ Attr.style "width" ( toPx space_width )
          , Attr.style "margin-left" ( toPx space_left )
          ]
        )
        [ text "space" ]
    button_container_height =
      viewParams.button_height + 2 * ( viewParams.button_stroke + viewParams.button_margin )
    button_stack_width =
      3 * viewParams.button_height + 6 * viewParams.button_stroke + 4 * viewParams.button_margin
    space_text_left =
      space_left + viewParams.button_margin
    space_text_width =
      space_width + 2 * viewParams.button_stroke
    small_button_height = viewParams.button_height / 2 - viewParams.button_stroke
    svg_arrow color angle =
      let svgsize = "20px" in
      div
        [ Attr.style "margin" "0 auto"
        , Attr.style "width" svgsize
        , Attr.style "height" svgsize
        ]
        [
          Svg.svg
            [ width svgsize
            , height svgsize ]
            [ Svg.polygon
                [ points "5,5 5,15 15,10"
                , fillOpacity "0.0"
                , stroke color
                , strokeWidth "2"
                , transform ( "rotate(" ++ ( String.fromFloat angle ) ++ " 10,10)" ) ]
                []
            ]
        ]
    small_botton bdiv color =
      div
        (
          button_attributes color ++
          font_attributes         ++
          [ Attr.style "height" ( toPx small_button_height )
          , Attr.style "margin" "0 auto"
          ]
        )
        [ bdiv ]
    arrow_buttons color =
      div
        [ Attr.style "height" ( toPx button_container_height )
        , Attr.style "margin-left" "100px"
        , Attr.style "float" "left" ]
        [ div
            ( button_attributes color ++ font_attributes ++ [ Attr.style "margin-right" "0" ] )
            [ svg_arrow c_a 180 ]
        , div
            [ Attr.style "display" "flex"
            , Attr.style "float" "left"
            , Attr.style "flex-direction" "column"
            , Attr.style "height" ( toPx button_container_height )
            , Attr.style "margin" "0"
            , Attr.style "margin-top" ( toPx viewParams.button_margin )
            ]
            [ small_botton ( svg_arrow c_a -90 ) c_a    -- Leaving unicode arrow here
            , small_botton ( svg_arrow c_a  90 ) c_a    -- in case we need them again: "↑↓←→"
            ]
        , div
            ( button_attributes color ++ font_attributes ++ [ Attr.style "margin-left" "0" ] )
            [ svg_arrow c_a 0 ]
        ]
    c_a = "#c0c0c0"
    c_b = "#606060"
  in
  [ div
      (
        font_attributes ++
        [ Attr.style "margin-top" ( toPx 24 )
        , Attr.style "text-align" "center"
        ]
      )
      [ text ( "Perspective Mode: " ++ mode_text ) ]
  , div
      [ Attr.style "width" ( toPx viewParams.window_width )
      , Attr.style "height" ( toPx button_container_height )
      , Attr.style "margin-top" ( toPx 8 )
      , Attr.style "margin-left" ( toPx 20 )
      , Attr.style "overflow" "hidden"
      ]
      [ number_button 0 ( if mode == Iso then c_b else c_a )
      , number_button 1 ( if mode == OnePoint then c_b else c_a )
      , number_button 2 c_a
      , space_button c_a
      , arrow_buttons c_a
      ]
  , div
      (
        font_attributes ++
        [ Attr.style "width" ( toPx viewParams.window_width )
        , Attr.style "height" ( toPx 20 )
        , Attr.style "margin-top" ( toPx 4 )
        , Attr.style "margin-left" ( toPx 20 )
        , Attr.style "overflow" "hidden"
        , Attr.style "font-size" ( toPx 18 )
        , Attr.style "color" c_a
        , Attr.style "text-align" "center"
        ]
      )
      [ div
          [ Attr.style "float" "left"
          , Attr.style "width" ( toPx button_stack_width) ]
          [ text "select mode" ]
      , div
          [ Attr.style "margin-left" ( toPx space_text_left )
          , Attr.style "width" ( toPx space_text_width )
          , Attr.style "float" "left" ]
          [ text "center camera" ]
      , div
          [ Attr.style "margin-left" ( toPx space_left )
          , Attr.style "width" ( toPx button_stack_width)
          , Attr.style "float" "left" ]
          [ text "move camera" ]
      ]
  ]


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
    s1 = viewParams.frame_outside
    s2 = viewParams.frame_inside
    x1 = -s1 / 2
    y1 = -s1 / 2
    z1 = 0
    n = 4
    z_spacing = vectorParams.z_spacing
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


makeCross z =
  let
    sh = viewParams.stroke_halfwidth
    cl = viewParams.cross_length
    vlist =
      [ ( vector sh cl z ), ( vector sh sh z ), ( vector cl sh z )
      , ( vector cl -sh z ), ( vector sh -sh z ), ( vector sh -cl z )
      , ( vector -sh -cl z ), ( vector -sh -sh z ), ( vector -cl -sh z )
      , ( vector -cl sh z ), ( vector -sh sh z ), ( vector -sh cl z )
      ]
  in
  makePolygon vlist


makeCrossStack =
  let
    n = 2
    spacing = 2 * vectorParams.z_spacing
  in
  ( List.range 0 n )
      |> List.map ( \z -> toFloat ( -z * spacing ) )
      |> List.map makeCross


getObjects : List ( PolygonM )
getObjects =
  makeFrameStack ++
  makeCrossStack
