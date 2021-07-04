module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Tuple exposing (pair)
import Task
import WebGL as WGL
import Array exposing (Array)
import WebGL.Texture as WGLTexture exposing (Texture)
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Debug

type alias Flags = ()

type Msg
  = TextureLoaded (Result WGLTexture.Error Texture)

type alias Tile = Int
type alias Chunk = Array Int

type alias Vertex =
  { position: Vec2
  , uvTileCoords: Vec2
  , uvRailCoords: Vec2
  , texCoords: Vec2
  }

type alias TileMeshUniforms =
  { texture: WGLTexture.Texture
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

type TileType
  = TileType_Flat
  | TileType_LDRU
  | TileType_LMRM
  | TileType_LURD
  | TileType_RDLU
  | TileType_RMLM
  | TileType_RULD
  | TileType_MDMU
  | TileType_MUMD

type RailType
  = RailType_None
  | RailType_LDRU
  | RailType_LMRM
  | RailType_LURD
  | RailType_RDLU
  | RailType_RMLM
  | RailType_RULD
  | RailType_MDMU
  | RailType_MUMD

createVertex: (Float, Float) -> CornerType -> TileType -> RailType -> Vertex
createVertex (posX, posY) cornerType tileType railType =
  let
    tileTypeToTexCoord = (\_ -> vec2 1 2)
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

tileToTriangles: Int -> Tile -> List (Vertex, Vertex, Vertex)
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

debugVertex: Vertex -> Vertex
debugVertex a =
  let
    _ = Debug.log "Position" ( Vector2.getX a.position, Vector2.getY a.position )
  in
    a

debugTriangle: (Vertex, Vertex, Vertex) -> (Vertex, Vertex, Vertex)
debugTriangle (a, b, c) =
  ( debugVertex a
  , debugVertex b
  , debugVertex c
  )

generateMesh: Chunk -> WGL.Mesh Vertex
generateMesh chunk =
  chunk
    |> Array.indexedMap (\ind tile -> (ind, tile))
    |> Array.toList
    |> List.concatMap (\(ind, tile) -> tileToTriangles ind tile)
--    |> List.map (debugTriangle)
    |> WGL.triangles

type alias Model =
  { mapSize: (Int, Int)
  , texture: Maybe Texture
  , chunk: Chunk
  , errorMessage: Maybe String
  }

createDefaultChunk: Chunk
createDefaultChunk = Array.repeat 32 0

initialModel: Model
initialModel =
  { mapSize = (500, 500)
  , texture = Nothing
  , chunk = createDefaultChunk
  , errorMessage = Nothing
  }

requestTexture: String -> Cmd Msg
requestTexture path =
  let
    defaultOptions = WGLTexture.defaultOptions
    loadOptions = { defaultOptions | flipY = False, magnify = WGLTexture.nearest }
  in
    WGLTexture.loadWith loadOptions path
      |> Task.attempt TextureLoaded

init: Flags -> (Model, Cmd Msg)
init _ =
  pair initialModel <| requestTexture "../resources/texture.png"

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TextureLoaded mbTexture ->
      case mbTexture of
         Ok texture -> ( {model | texture = Just texture}, Cmd.none )
         Err _ -> ( {model | errorMessage = Just "Cant load texture"}, Cmd.none )

view: Model -> Html Msg
view model =
  case model.texture of
    Just texture ->
      let
        chunkShifts =
          [ (0.0, 0.0), (1.0, 0.0)
          , (0.0, 1.0), (1.0, 1.0)
          , (0.0, 2.0), (1.0, 2.0)
          , (0.0, 3.0), (1.0, 3.0)
          ]
        entityGenerator: TileMeshUniforms -> WGL.Entity
        entityGenerator uniform =
          WGL.entity
              vertTileShader
              fragTileShader
              (generateMesh model.chunk)
              uniform
        entities =
          chunkShifts
            |> List.map (\(shiftX, shiftY) -> { texture = texture, chunkShift = vec2 shiftX shiftY })
            |> List.map entityGenerator
      in
      WGL.toHtml
        [ width 1200
        , height 500
        , style "border" "1px solid black"
        ] entities
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

    uniform sampler2D texture;

    varying vec2 uvTexCoords;

    void main() {
      gl_FragColor = texture2D(texture, uvTexCoords);
    }
  |]