module BitOps exposing (getBits)

import Bitwise exposing (shiftRightBy, shiftLeftBy)

getBits: Int -> Int -> Int -> Int
getBits offset size source = Bitwise.and (shiftRightBy source offset) (shiftLeftBy size 1)