module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Tuple exposing (pair)
import Task
import WebGL as WGL
import Array exposing (Array)
import WebGL.Texture as WGLTexture
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Debug

import ArrayChunkStorage
import Tile exposing (Tile, TileType(..), RailType(..))
import Chunk

type alias Flags = ()

type Msg
  = TextureLoaded (Result WGLTexture.Error WGLTexture.Texture)

type alias Vertex =
  { position: Vec2
  , uvTileCoords: Vec2
  , uvRailCoords: Vec2
  , texCoords: Vec2
  }

type alias TileMeshUniforms =
  { tilesetTexture: WGLTexture.Texture
  , chunkShift: Vec2
  }

type alias TileMeshVaryings =
  { uvTexCoords: Vec2
  }

type CornerType
  = CornerType_LD
  | CornerType_LU
  | CornerType_RD
  | CornerType_RU

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

type alias Map =
  { mapSize: (Int, Int)
  , chunks: ArrayChunkStorage
  }

type alias RenderingParams =
  { canvasSize: (Int, Int)
  , zoom: Float
  , mapOffset: (Int, Int)
  , tilesetTexture: Maybe WGLTexture.Texture
  }

type alias Model =
  { map: Map
  , renderingParams: RenderingParams
  , errorMessage: Maybe String
  }

type alias ArrayChunkStorageCell =
  { horizontalOffset: Int
  , verticalOffset: Int
  , chunk: Chunk.Chunk
  }

type alias ArrayChunkStorage = Array ArrayChunkStorageCell

generateSampleMap: Map
generateSampleMap =
  let
    mapSize = pair 800 400
    chunks = ArrayChunkStorage.createDummyStorage (800, 400)
  in
    { mapSize = mapSize
    , chunks = chunks
    }

initialModel: Model
initialModel =
  { map = generateSampleMap
  , renderingParams =
      { canvasSize = (1000, 500)
      , zoom = 1.0
      , mapOffset = (0, 0)
      , tilesetTexture = Nothing
      }
  , errorMessage = Nothing
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
  pair initialModel <| requestTexture "./texture.png"

setTexture: WGLTexture.Texture -> Model -> Model
setTexture texture model =
  let
    renderingParams = model.renderingParams
    updatedParams = { renderingParams | tilesetTexture = Just texture }
  in
    { model | renderingParams = updatedParams }

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TextureLoaded mbTexture ->
      case mbTexture of
         Ok texture ->
           pair
             ( setTexture texture model )
             Cmd.none
         Err _ -> ( {model | errorMessage = Just "Cant load texture"}, Cmd.none )

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

renderChunk: WGLTexture.Texture -> (Int, Int, Chunk.Chunk) -> WGL.Entity
renderChunk texture (hOffset, vOffset, chunk) =
  WGL.entity vertTileShader fragTileShader (generateMesh chunk)
    { tilesetTexture = texture
    , chunkShift = vec2 (toFloat hOffset) (toFloat vOffset)
    }

view: Model -> Html Msg
view model =
  case model.renderingParams.tilesetTexture of
    Just texture ->
      let
        (canvasWidth, canvasHeight) = model.renderingParams.canvasSize
      in
      WGL.toHtml [ width canvasWidth , height canvasHeight ]
        ( collectVisibleChunks (0, 10) (0, 10) model.map.chunks
            |> Array.toList
            |> Debug.log "List"
            |> List.map (renderChunk texture )
        )
    Nothing ->
      Html.text <|
        case model.errorMessage of
           Just message -> message
           Nothing -> "Hz voobs4e"


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
    precision highp float;

    attribute vec2 position;
    attribute vec2 uvTileCoords;
    attribute vec2 uvRailCoords;
    attribute vec2 texCoords;

    uniform vec2 chunkShift;

    varying vec2 uvTexCoords;

    void main() {
      vec2 mapCoords = vec2(position.x + chunkShift.x * 8.0, position.y + chunkShift.y * 4.0) * 0.1 - 1.0;
      gl_Position = vec4(mapCoords, 0.0, 1.0);
      uvTexCoords = (texCoords + uvTileCoords) * 0.25;
    }
  |]

fragTileShader: WGL.Shader {} TileMeshUniforms TileMeshVaryings
fragTileShader =
  [glsl|
    precision highp float;

    uniform sampler2D tilesetTexture;

    varying vec2 uvTexCoords;

    void main() {
      gl_FragColor = texture2D(tilesetTexture, uvTexCoords);
    }
  |]