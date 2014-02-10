module Image (isPNG, isJPG, isSVG, svgSize) where

import Image.PNG (isPNG, pngSize)
import Image.JPG (isJPG, jpgSize)
import Image.SVG (isSVG, svgSize)
