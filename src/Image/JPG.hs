{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module Image.JPG (isJPG) where

import Data.Maybe
import File.Binary
import File.Binary.Instances
import File.Binary.Instances.BigEndian

isJPG :: String -> Bool
isJPG jpg = isJust (fromBinary () jpg :: Maybe (JPG, String))

[binary|

JPG deriving Show

1: 0xff
1: 0xd8
replicate 9 () {[Segment]}: segments

|]

[binary|

Segment deriving Show

1: 0xff
1: marker
2: size
replicate (size - 2) (){String}: contents

|]
