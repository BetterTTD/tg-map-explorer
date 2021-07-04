module Tile exposing (Tile, TileType, RailType, getTileType, getRailType)

import BitOps exposing (getBits)

type alias Tile = Int

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
  | TileType_Invalid

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
  | RailType_Invalid

getTileType: Tile -> TileType
getTileType tile =
  case getBits 1 3 tile of
    0 -> TileType_Flat
    1 -> TileType_LDRU
    2 -> TileType_LMRM
    3 -> TileType_LURD
    4 -> TileType_RDLU
    5 -> TileType_RMLM
    6 -> TileType_RULD
    7 -> TileType_MDMU
    8 -> TileType_MUMD
    _ -> TileType_Invalid

getRailType: Tile -> RailType
getRailType tile =
  case getBits 4 3 tile of
    0 -> RailType_None
    1 -> RailType_LDRU
    2 -> RailType_LMRM
    3 -> RailType_LURD
    4 -> RailType_RDLU
    5 -> RailType_RMLM
    6 -> RailType_RULD
    7 -> RailType_MDMU
    8 -> RailType_MUMD
    _ -> RailType_Invalid