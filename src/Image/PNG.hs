{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module Image.PNG (isPNG) where

import Data.Maybe
import File.Binary
import File.Binary.Instances
import File.Binary.Instances.BigEndian

isPNG :: String -> Bool
isPNG img = isJust (fromBinary () img :: Maybe (PNGHeader, String))

[binary|

PNGHeader deriving Show

1: 0x89
3: "PNG"
2: "\r\n"
1: "\SUB"
1: "\n"

|]
