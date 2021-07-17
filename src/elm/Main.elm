module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (width, height)
import Html.Events as HtmlEvents
import Tuple exposing (pair)
import Task
import WebGL as WGL
import Array exposing (Array)
import WebGL.Texture as WGLTexture
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Math.Matrix4 as Matrix4 exposing (Mat4)

import Json.Decode as Decode
import Debug

import ArrayChunkStorage
import Tile exposing (TileType(..), RailType(..))
import Chunk

type alias Flags = ()

type WheelMove
  = WheelMoveUp
  | WheelMoveDown

type MouseEventMsg
  = MouseDown MousePosition
  | MouseUp MousePosition
  | MouseMove MousePosition
  | MouseWheel WheelMove

type Msg
  = TextureLoaded (Result WGLTexture.Error WGLTexture.Texture)
  | MouseEvent MouseEventMsg

type alias Vertex =
  { position: Vec2
  , uvTileCoords: Vec2
  , uvRailCoords: Vec2
  , texCoords: Vec2
  }

type alias TileMeshUniforms =
  { tilesetTexture: WGLTexture.Texture
  , perspective: Mat4
  }

type alias TileMeshVaryings =
  { uvTexCoords: Vec2
  }

type CornerType
  = CornerType_LD
  | CornerType_LU
  | CornerType_RD
  | CornerType_RU


tilesetTexturePath: String
tilesetTexturePath = "./texture.png"

type CanvasSize =
  CanvasSize Int Int

canvasSize: Int -> Int -> CanvasSize
canvasSize w h = CanvasSize w h

getCanvasWidth: CanvasSize -> Int
getCanvasWidth (CanvasSize w _) = w

getCanvasHeight: CanvasSize -> Int
getCanvasHeight (CanvasSize _ h) = h

createVertex: (Float, Float) -> CornerType -> Tile.TileType -> Tile.RailType -> Vertex
createVertex (posX, posY) cornerType tileType railType =
  let
    tileTypeToTexCoord = (\_ -> vec2 1 1)
    railTypeToTextCoord = (\_ -> vec2 0 0)
  in
    { position = vec2 posX posY
    , uvTileCoords = tileTypeToTexCoord tileType
    , uvRailCoords = railTypeToTextCoord railType
    , texCoords =
        case cornerType of
          CornerType_LD -> vec2 0 0
          CornerType_LU -> vec2 0 1
          CornerType_RD -> vec2 1 0
          CornerType_RU -> vec2 1 1
    }

tileToTriangles: Int -> Tile.Tile -> List (Vertex, Vertex, Vertex)
tileToTriangles ind _ =
  let

    xLD = modBy 8 ind |> toFloat
    yLD = 3 - ind // 8 |> toFloat

    (xLU, xRU, xRD) = (xLD, xLD + 1.0, xLD + 1.0)
    (yLU, yRU, yRD) = (yLD + 1.0, yLD + 1.0, yLD)
    vrtLD = createVertex (xLD, yLD) CornerType_LD TileType_Flat RailType_None
    vrtLU = createVertex (xLU, yLU) CornerType_LU TileType_Flat RailType_None
    vrtRD = createVertex (xRD, yRD) CornerType_RD TileType_Flat RailType_None
    vrtRU = createVertex (xRU, yRU) CornerType_RU TileType_Flat RailType_None
  in
    [ ( vrtLD, vrtLU, vrtRU )
    , ( vrtLD, vrtRD, vrtRU )
    ]

type MapSize =
  MapSize Int Int

createMapSize: Int -> Int -> MapSize
createMapSize h w = MapSize w h

getMapWidth: MapSize -> Int
getMapWidth (MapSize _ value) =
  value

getMapHeight: MapSize -> Int
getMapHeight (MapSize value _) =
  value

type alias Map =
  { mapSize: MapSize
  , chunks: ArrayChunkStorage
  }

type ZoomFactor =
  ZoomFactor Float

zoomFactor: Float -> ZoomFactor
zoomFactor factor =
  ZoomFactor factor

type MapOffset =
  MapOffset Float Float

getHorizontalMapOffset: MapOffset -> Float
getHorizontalMapOffset (MapOffset value _) =
  value

getVerticalMapOffset: MapOffset -> Float
getVerticalMapOffset (MapOffset _ value) =
  value

type ViewportOffset =
  ViewportOffset Int Int

viewportOffset: Int -> Int -> ViewportOffset
viewportOffset x y =
  ViewportOffset x y

type alias RenderingParams =
  { canvasSize: CanvasSize
  , zoom: ZoomFactor
  , mapOffset: MapOffset
  , viewportOffset: ViewportOffset
  , tilesetTexture: Maybe WGLTexture.Texture
  }

type MousePosition = MousePosition Int Int

mousePosition: Int -> Int -> MousePosition
mousePosition x y = MousePosition x y

getMousePositionX: MousePosition -> Int
getMousePositionX (MousePosition x _) = x

getMousePositionY: MousePosition -> Int
getMousePositionY (MousePosition _ y) = y

type alias MouseControlState =
  { lastMousePosition: Maybe MousePosition
  }

type alias Model =
  { map: Map
  , renderingParams: RenderingParams
  , mouseControlState: MouseControlState
  }

type alias ArrayChunkStorageCell =
  { horizontalOffset: Int
  , verticalOffset: Int
  , chunk: Chunk.Chunk
  }

type alias ArrayChunkStorage = Array ArrayChunkStorageCell

generateSampleMap: MapSize -> Map
generateSampleMap mapSize =
  let
    mapWidth = getMapWidth mapSize
    mapHeight = getMapHeight mapSize
    chunks = ArrayChunkStorage.createDummyStorage (mapWidth, mapHeight)
  in
    { mapSize = mapSize
    , chunks = chunks
    }

initialModel: Model
initialModel =
  { map = generateSampleMap <| createMapSize 800 400
  , renderingParams =
      { canvasSize = canvasSize 1200 600
      , zoom = zoomFactor 100
      , mapOffset = MapOffset 0.0 0.0
      , viewportOffset = viewportOffset 0 0
      , tilesetTexture = Nothing
      }
  , mouseControlState =
      { lastMousePosition = Nothing
      }
  }

requestTexture: String -> Cmd Msg
requestTexture path =
  let
    defaultOptions = WGLTexture.defaultOptions
    loadOptions = { defaultOptions | flipY = True, magnify = WGLTexture.nearest }
  in
    WGLTexture.loadWith loadOptions path
      |> Task.attempt TextureLoaded

init: Flags -> (Model, Cmd Msg)
init _ =
  pair initialModel <| requestTexture tilesetTexturePath

setTexture: WGLTexture.Texture -> Model -> Model
setTexture texture model =
  let
    renderingParams = model.renderingParams
    updatedParams = { renderingParams | tilesetTexture = Just texture }
  in
    { model | renderingParams = updatedParams }

setLastMousePosition: Maybe MousePosition -> MouseControlState -> MouseControlState
setLastMousePosition value state =
  { state | lastMousePosition = value }

updateMapOffset: Float -> Float -> RenderingParams -> RenderingParams
updateMapOffset shiftX shiftY params =
  let
    (MapOffset oldXOffset oldYOffset) = params.mapOffset
  in
  { params
  | mapOffset = MapOffset (oldXOffset + shiftX) (oldYOffset - shiftY)
  }

increaseZoom: RenderingParams -> RenderingParams
increaseZoom params =
  let
    (ZoomFactor zoom) = params.zoom
  in
    if zoom <= 90
      then
        { params | zoom = ZoomFactor <| zoom + 10}
      else
        params

decreaseZoom: RenderingParams -> RenderingParams
decreaseZoom params =
  let
    (ZoomFactor zoom) = params.zoom
  in
    if zoom > 10
      then
        { params | zoom = ZoomFactor <| zoom - 10}
      else
        params


handleMouseMsg: MouseEventMsg -> Model -> (Model, Cmd Msg)
handleMouseMsg msg model =
  case msg of
    MouseUp _ ->
      pair
        { model | mouseControlState = setLastMousePosition Nothing model.mouseControlState }
        Cmd.none

    MouseDown position ->
      pair
        { model | mouseControlState = setLastMousePosition (Just position) model.mouseControlState }
        Cmd.none

    MouseWheel wheelType ->
      case wheelType of
        WheelMoveUp ->
          ( { model | renderingParams = increaseZoom model.renderingParams }
          , Cmd.none
          )

        WheelMoveDown ->
          ( { model | renderingParams = decreaseZoom model.renderingParams }
          , Cmd.none
          )

    MouseMove position ->
      case model.mouseControlState.lastMousePosition of
        Nothing ->
          (model, Cmd.none)
        Just (MousePosition lastX lastY)->
          let
            (ZoomFactor zoom) = model.renderingParams.zoom
            (MousePosition curX curY) = position
            shiftX = toFloat (lastX - curX) / zoom
            shiftY = toFloat (lastY - curY) / zoom
          in
          ( { model
            |   mouseControlState = setLastMousePosition (Just position) model.mouseControlState
            ,   renderingParams = updateMapOffset shiftX shiftY model.renderingParams
            }
          , Cmd.none
          )

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    TextureLoaded mbTexture ->
      case mbTexture of
         Ok texture ->
           pair
             ( setTexture texture model )
             Cmd.none
         Err _ -> pair model Cmd.none

    MouseEvent mouseMsg ->
      handleMouseMsg mouseMsg model

collectVisibleChunks: (Int, Int) -> (Int, Int) -> ArrayChunkStorage -> Array (Int, Int, Chunk.Chunk)
collectVisibleChunks hLim vLim storage =
  ArrayChunkStorage.getVisibleChunks hLim vLim storage

generateMesh: Chunk.Chunk -> WGL.Mesh Vertex
generateMesh chunk =
  chunk
    |> Array.indexedMap (\ind tile -> (ind, tile))
    |> Array.toList
    |> List.concatMap (\(ind, tile) -> tileToTriangles ind tile)
    |> WGL.triangles

renderChunk: WGLTexture.Texture -> Mat4 -> (Int, Int, Chunk.Chunk) -> WGL.Entity
renderChunk texture perspective (hOffset, vOffset, chunk) =
  let
    translationX = toFloat hOffset * 8.0
    translationY = toFloat vOffset * 4.0
    translationZ = 0.0
  in
  WGL.entity vertTileShader fragTileShader (generateMesh chunk)
    { tilesetTexture = texture
    , perspective =
        Matrix4.translate3 translationX translationY translationZ perspective
    }

mouseOffsetXDecoder: Decode.Decoder Int
mouseOffsetXDecoder =
  Decode.field "offsetX" Decode.int

mouseOffsetYDecoder: Decode.Decoder Int
mouseOffsetYDecoder =
  Decode.field "offsetY" Decode.int

mouseOffsetDecoder: Decode.Decoder MousePosition
mouseOffsetDecoder =
  Decode.map2 mousePosition mouseOffsetXDecoder mouseOffsetYDecoder

mouseMoveDecoder: Decode.Decoder Msg
mouseMoveDecoder =
  Decode.map (MouseEvent << MouseMove) mouseOffsetDecoder

mouseUpDecoder: Decode.Decoder Msg
mouseUpDecoder =
  Decode.map (MouseEvent << MouseUp) mouseOffsetDecoder

mouseDownDecoder: Decode.Decoder Msg
mouseDownDecoder =
  Decode.map (MouseEvent << MouseDown) mouseOffsetDecoder

deltaYDecoder: Decode.Decoder Int
deltaYDecoder =
  Decode.field "deltaY" Decode.int

deltaYToWheelMove: Int -> WheelMove
deltaYToWheelMove value =
  if value > 0
    then
      WheelMoveUp
    else
      WheelMoveDown

wheelDecoder =
  Decode.map (MouseEvent << MouseWheel << deltaYToWheelMove) deltaYDecoder

view: Model -> Html Msg
view model =
  case model.renderingParams.tilesetTexture of
    Just texture ->
      let
        renderingParams = model.renderingParams
        canvasWidth = getCanvasWidth renderingParams.canvasSize
        canvasHeight = getCanvasHeight renderingParams.canvasSize
        hMapOffset = getHorizontalMapOffset renderingParams.mapOffset
        vMapOffset = getVerticalMapOffset renderingParams.mapOffset
        (ZoomFactor zoom) = model.renderingParams.zoom
        horizontalTilesCount = ceiling <| toFloat canvasWidth / zoom
        vertiacalTilesCoutn = ceiling <| toFloat canvasHeight / zoom

        -- Границы отображаемого сегмента карты в системе координат карты
        leftXBorder = hMapOffset
        downYBorder = vMapOffset
        rightXBorder = leftXBorder + (toFloat horizontalTilesCount)
        upYBorder = downYBorder + (toFloat vertiacalTilesCoutn)

        leftChunkOffset = floor leftXBorder // 8
        downChunkOffset = floor downYBorder // 4

        perspective = Matrix4.makeOrtho2D leftXBorder rightXBorder downYBorder upYBorder
        visibleChunks =
          collectVisibleChunks
            ( floor leftXBorder, ceiling rightXBorder)
            ( floor downYBorder, ceiling upYBorder)
            model.map.chunks
      in
      WGL.toHtml
        [ width canvasWidth
        , height canvasHeight
        , HtmlEvents.on "mousedown" mouseDownDecoder
        , HtmlEvents.on "mousemove" mouseMoveDecoder
        , HtmlEvents.on "mouseup" mouseUpDecoder
        , HtmlEvents.on "wheel" wheelDecoder
        , Html.Attributes.style "border" "1px solid black"
        ]
        ( visibleChunks
            |> Array.map (\(hOffset, vOffset, chunk) -> (hOffset // 8 - leftChunkOffset, vOffset // 4 - downChunkOffset, chunk))
            |> Array.toList
            |> List.map (renderChunk texture perspective)
        )
    Nothing ->
      Html.text "Freeze"


subscriptions: Model -> Sub Msg
subscriptions _ = Sub.none

main: Program Flags Model Msg
main = Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

vertTileShader: WGL.Shader Vertex TileMeshUniforms TileMeshVaryings
vertTileShader = 
  [glsl|
    precision mediump float;

    attribute vec2 position;
    attribute vec2 uvTileCoords;
    attribute vec2 uvRailCoords;
    attribute vec2 texCoords;

    uniform mat4 perspective;

    varying vec2 uvTexCoords;

    void main() {
      gl_Position = perspective * vec4(position, 0.0, 1.0);
      uvTexCoords = (texCoords + uvTileCoords) * 0.25;
    }
  |]

fragTileShader: WGL.Shader {} TileMeshUniforms TileMeshVaryings
fragTileShader =
  [glsl|
    precision mediump float;

    uniform sampler2D tilesetTexture;

    varying vec2 uvTexCoords;

    void main() {
      gl_FragColor = texture2D(tilesetTexture, uvTexCoords);
    }
  |]