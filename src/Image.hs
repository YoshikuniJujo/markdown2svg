module Image (convertedSize, Size(..)) where

import Control.Arrow
import Image.PNG (isPNG, pngSize)
import Image.JPG (isJPG, jpgSize)
import Image.SVG (isSVG, svgSize)

data Size = Small | Medium | Large deriving Show

imageSize :: String -> Maybe (Double, Double)
imageSize src = case (isPNG src, isJPG src, isSVG src) of
		(True, _, _) -> pngSize src
		(_, True, _) -> jpgSize src
		(_, _, True) -> svgSize src
		_ -> Nothing

convertedSize :: String -> Size -> Maybe (Double, Double)
convertedSize src sml = case imageSize src of
		Just sz -> Just $ convert sz sml
		_ -> Nothing

root2 :: Double
root2 = 2 ** (1 / 2)

convert :: (Double, Double) -> Size -> (Double, Double)
convert (w, h) Large
	| w * root2 > h = (1, h / w)
	| otherwise = (w * root2 / h, root2)
convert sz Medium = (/ 3) *** (/ 3) $ convert sz Large
convert sz Small = (/ 10) *** (/ 10) $ convert sz Large
