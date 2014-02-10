{-# LANGUAGE QuasiQuotes, TypeFamilies, PackageImports, DoAndIfThenElse #-}

module Image.JPG (isJPG, jpgSize) where

import Control.Applicative
import Control.Arrow
import "monads-tf" Control.Monad.State

import Data.Maybe
import Data.Monoid

import File.Binary
import File.Binary.Instances
import File.Binary.Instances.BigEndian

isJPG :: String -> Bool
isJPG jpg = isJust (fromBinary () jpg :: Maybe (JPG, String))

jpgSize :: String -> Maybe (Double, Double)
jpgSize jpg = (,)
	<$> (fromIntegral . width <$> j)
	<*> (fromIntegral . height <$> j)
	where
	j = fst <$> fromBinary () jpg

newtype List a = List [a] deriving Show

instance Field a => Field (List a) where
	type FieldArgument (List a) = (a -> Bool, FieldArgument a)
	fromBits (p, a) ab = do
		(f, rest) <- fromBits a ab
		if p f
		then do	(List fs, rest') <- fromBits (p, a) rest
			return (List $ f : fs, rest')
		else do return (List [], ab)
	consToBits = error "not defined"

[binary|

JPG deriving Show

1: 0xff
1: 0xd8
iscxa {LSegment}: segments
1: 0xff
1: cx
2: cxSize
1: 8
2: height
2: width

|]

type LSegment = List Segment

iscxa = (not . iscx, ())

iscx :: Segment -> Bool
iscx Segment{ marker = m, eight = e } =
	m >= 0xc0 && m <= 0xcf && e == 8

[binary|

Segment deriving Show

1: 0xff
1: marker
2: size
1: eight
replicate (size - 3) (){String}: contents

|]
