module SVG (
	textToSVG
) where

import Control.Applicative
import Data.Maybe
import Data.Char
import System.FilePath
import Text.XML.YJSVG hiding (Image)
import qualified Text.XML.YJSVG as SVG

import Text.Markdown.Pap
import SepWords
import Image

type FontsS = (String, String, String)
type Fonts = (Font, Font, Font)

-- headerFont, normalFont, codeFont :: Font
-- headerFont = Font "Kochi Gothic" Normal
-- normalFont = Font "Kochi Mincho" Normal
-- headerFont = Font "Sans" Normal
-- normalFont = Font "Serif" Normal
-- normalFont = "Kochi Gothic"
-- codeFont = "Kochi Gothic"
codeFont = Font "Monospace" Normal

textToSVG :: FontsS -> [(FilePath, String)] -> Bool -> Double -> [Text] -> [String]
textToSVG (hf, nf, cf) fp n r =
	map (showSVG (width r) (height r)) .
	textToSVGData (Font hf Normal, Font nf Normal, Font cf Normal) fp r (topMargin r) .
	preprocess .
	if n then addChapters [Nothing, Just 0, Just 0, Just 0, Nothing, Nothing] else id

width, height :: Double -> Double
width = (3535 *)
height = (5000 *)

topMargin, leftMargin, paraLeftMargin, codeLeftMargin, listLeftMargin
	:: Double -> Double
topMargin = (400 *)
leftMargin = (400 *)
paraLeftMargin = (+) <$> leftMargin <*> (200 *)
codeLeftMargin = (+) <$> paraLeftMargin <*> (200 *)
listLeftMargin = (+) <$> paraLeftMargin <*> (200 *)

bottomMargin :: Double -> Double
bottomMargin = (400 *)

bottomBorder :: Double -> Double
bottomBorder = (-) <$> height <*> bottomMargin

header :: Int -> Double -> Double
header i = ((1 / fromIntegral (i + 1)) * 450 *)
headerSep i = (* (4 / 3)) <$> header i

normal, normalSep :: Double -> Double
normal = (80 *)
normalSep = (* (4 / 3)) <$> normal

code, codeSep :: Double -> Double
code = (65 *)
codeSep = (* (4 / 3)) <$> code

lineChars :: Int
lineChars = 60

textToSVGData :: Fonts -> [(FilePath, String)] -> Double -> Double -> [Text] -> [[SVG]]
textToSVGData fs@(hf, nf, cf) fp r h [] = [[]]
textToSVGData fs@(hf, nf, cf) fp r h (Header n s : ts@(List l : _))
	| h' > bottomBorder r = [] : (ln : one') : rest'
	where
	(_, h', _) = listToSVGData nf 1 "*+-------" r (h + headerSep n r * 5 / 4 + 100 * r) (listLeftMargin r) l
	ln = Text (TopLeft (leftMargin r) (topMargin r + headerSep n r)) (header n r) (ColorName "black") hf s
	one' : rest' = textToSVGData fs fp r (topMargin r + headerSep n r * 5 / 4) ts
textToSVGData fs@(hf, nf, cf) fp r h (Header n s : ts@(Paras (p : _) : _))
	| h' > bottomBorder r = [] : (l : one') : rest'
	where
	(h', svgs) = paraToSVGData nf r (h + headerSep n r * 5 / 4) p
	l = Text (TopLeft (leftMargin r) (topMargin r + headerSep n r)) (header n r) (ColorName "black") hf s
	one' : rest' = textToSVGData fs fp r (topMargin r + headerSep n r * 5 / 4) ts
textToSVGData fs@(hf, nf, cf) fp r h (Header n s : ts@(i@(Image _ _ _) : _))
	| h + headerSep n r * 5 / 4 + getImageHeight fp r i > bottomBorder r =
		[] : (l : one') : rest'
	where
	l = Text (TopLeft (leftMargin r) (topMargin r + headerSep n r)) (header n r) (ColorName "black") hf s
	one' : rest' = textToSVGData fs fp r (topMargin r + headerSep n r * 5 / 4) ts
textToSVGData fs@(hf, nf, cf) fp r h (Header n s : ts)
	| h > bottomBorder r - headerSep n r = [] : (l' : one') : rest'
	| otherwise = (l : one) : rest
	where
	l = Text (TopLeft (leftMargin r) (h + headerSep n r)) (header n r) (ColorName "black") hf s
	l' = Text (TopLeft (leftMargin r) (topMargin r + headerSep n r)) (header n r) (ColorName "black") hf s
	one : rest = textToSVGData fs fp r (h + headerSep n r * 5 / 4) ts
	one' : rest' = textToSVGData fs fp r (topMargin r + headerSep n r * 5 / 4) ts
textToSVGData fs@(hf, nf, cf) fp r h (Paras [] : ts) = textToSVGData fs fp r h ts
textToSVGData fs@(hf, nf, cf) fp r h (Paras (p : ps) : ts)
	| h' > bottomBorder r = [] : (svgs' ++ one') : rest'
	| otherwise = (svgs ++ one) : rest
	where
	(h', svgs) = paraToSVGData nf r h p
	(h'', svgs') = paraToSVGData nf r (topMargin r) p
	one : rest = textToSVGData fs fp r h' (Paras ps : ts)
	one' : rest' = textToSVGData fs fp r h'' (Paras ps : ts)
textToSVGData fs@(hf, nf, cf) fp r h (List l : ts)
	| h' > bottomBorder r = [] : (svgs' ++ one') : rest'
	| otherwise = (svgs ++ one) : rest
	where
	(_, h', svgs) = listToSVGData nf 1 "*+-------" r (h + 100 * r) (listLeftMargin r) l
	(_, h'', svgs') = listToSVGData nf 1 "*+-------" r (topMargin r) (listLeftMargin r) l
	one : rest = textToSVGData fs fp r (h' + 100 * r) ts
	one' : rest' = textToSVGData fs fp r (h'' + 100 * r) ts
textToSVGData fs@(hf, nf, cf) fp r h (Code s : ts)
	| h' > bottomBorder r = [] : (svgs' ++ one') : rest'
	| otherwise = (svgs ++ one) : rest
	where
	(h', svgs) = codeToSVGData cf r (h + codeSep r) (lines s)
	(h'', svgs') = codeToSVGData cf r (topMargin r) (lines s)
	one : rest = textToSVGData fs fp r (h' + codeSep r) ts
	one' : rest' = textToSVGData fs fp r (h'' + codeSep r) ts
textToSVGData fs@(hf, nf, cf) fp r h (Image _ p ttl : ts)
	| h + ht > bottomBorder r =
		[] : (SVG.Image (TopLeft left (topMargin r)) wt ht p : svg') : svgs'
	| otherwise = (SVG.Image (TopLeft left h) wt ht p : svg) : svgs
	where
	svg : svgs = textToSVGData fs fp r (h + ht + r * 100) ts
	svg' : svgs' = textToSVGData fs fp r (topMargin r + ht + r * 100) ts
	size = case ttl of
		"small" -> Small
		"large" -> Large
		_ -> Medium
	(wt, ht) = case getSize fp p size of
		Just (w_, h_) -> (w_ * ratio, h_ * ratio)
		_ -> (r * 1000, r * 1000)
	ratio = width r - 2 * leftMargin r
	left = case ttl of
		"large" -> leftMargin r
		"small" -> ratio * 4 / 10
		_ -> ratio * 1 / 3
textToSVGData fs@(hf, nf, cf) fp r h (Link txt addrs ttl : ts)
	| h + normalSep r > bottomBorder r = [] : (l : svg') : svgs'
	| otherwise = (l : svg) : svgs
	where
	svg : svgs = textToSVGData fs fp r (h + normalSep r) ts
	svg' : svgs' = textToSVGData fs fp r (topMargin r) ts
	l = Text (TopLeft (paraLeftMargin r) (h + normalSep r)) (normal r) (ColorName "blue")
			nf $ txt ++ "(" ++ addrs ++ ")"

splitAtString :: Int -> String -> (String, String)
splitAtString len = sepStr 0
	where
	sepStr _ "" = ("", "")
	sepStr n (c : c'@('ー') : cs)
		|  n > len = ([c, c'], cs)
		| otherwise = let (s, t) = sepStr (n + 4) cs in (c : c' : s, t)
	sepStr n (c : c'@('。') : cs)
		|  n > len = ([c, c'], cs)
		| otherwise = let (s, t) = sepStr (n + 4) cs in (c : c' : s, t)
	sepStr n (c : cs)
		| n > len = ([c], cs)
		| isAscii c = let (s, t) = sepStr (n + 1) cs in (c : s, t)
		| otherwise = let (s, t) = sepStr (n + 2) cs in (c : s, t)

separateString :: Int -> String -> [String]
separateString len = sepStr 0
	where
	sepStr _ "" = [[]]
	sepStr n (c : c'@('ー') : cs)
		|  n > len = [c, c'] : sepStr 0 cs
		| otherwise = let s : ss = sepStr (n + 4) cs in (c : c' : s) : ss
	sepStr n (c : c'@('。') : cs)
		|  n > len = [c, c'] : sepStr 0 cs
		| otherwise = let s : ss = sepStr (n + 4) cs in (c : c' : s) : ss
	sepStr n (c : cs)
		| n > len = [c] : sepStr 0 cs
		| isAscii c = let s : ss = sepStr (n + 1) cs in (c : s) : ss
		| otherwise = let s : ss = sepStr (n + 2) cs in (c : s) : ss

paraToSVGData :: Font -> Double -> Double -> String -> (Double, [SVG])
paraToSVGData nf r h str = (h', l : svgs)
	where
	(s, t) = splitWords (lineChars - 3) str
	l = Text (TopLeft (paraLeftMargin r + normal r) (h + normal r)) (normal r) (ColorName "black") nf s
	ls = sepWords lineChars t
	(h', svgs) = strsToSVGData nf r (h + normalSep r) ls

strsToSVGData :: Font -> Double -> Double -> [String] -> (Double, [SVG])
strsToSVGData _ r h [] = (h, [])
strsToSVGData nf r h ([] : ss) = strsToSVGData nf r h ss
strsToSVGData nf r h (s : ss) = (h', l : svgs)
	where
	l = Text (TopLeft (paraLeftMargin r) (h + normal r)) (normal r) (ColorName "black") nf s
	(h', svgs) = strsToSVGData nf r (h + normalSep r) ss

listToSVGData :: Font -> Int -> String -> Double -> Double -> Double -> List -> (Int, Double, [SVG])
listToSVGData _ n sym r y x [] = (n, y, [])
listToSVGData nf n sym r y x (lst : lsts) = (n'', y'', svgs ++ svgs')
	where
	(n', y', svgs) = list1ToSVGData nf n sym r y x lst
	(n'', y'', svgs') = listToSVGData nf n' sym r y' x lsts

list1ToSVGData :: Font -> Int -> String -> Double -> Double -> Double -> List1 -> (Int, Double, [SVG])
list1ToSVGData nf n sym r y x (BulItem s lst) = (n, h', l : svgs)
	where
	l = Text (TopLeft x (y + normal r)) (normal r) (ColorName "black") nf $ head sym : ' ' : s
	(_, h', svgs) = listToSVGData nf n (tail sym) r (y + normalSep r) (x + normal r * 2) lst
list1ToSVGData nf n sym r y x (OrdItem s lst) = (n + 1, h', l : svgs)
	where
	l = Text (TopLeft x (y + normal r)) (normal r) (ColorName "black") nf $ show n ++ '.' : ' ' : s
	(_, h', svgs) = listToSVGData nf n (tail sym) r (y + normalSep r) (x + normal r * 2) lst

codeToSVGData :: Font -> Double -> Double -> [String] -> (Double, [SVG])
codeToSVGData _ _ h [] = (h, [])
codeToSVGData cf r h (s : ss) = (h', l : svgs)
	where
	l = Text
		(TopLeft
			(codeLeftMargin r + (code r) * spaces)
			(h + code r))
		(code r) (ColorName "black") cf s
	(h', svgs) = codeToSVGData cf r (h + codeSep r) ss
	spaces = (* (123 / 200)) $ fromIntegral $ length $ takeWhile (== ' ') s

addChapters :: [Maybe Int] -> [Text] -> [Text]
addChapters _ [] = []
addChapters cs (Header n s : ts)
	| isJust $ cs !! (n - 1) =
		Header n (chaps ++ " " ++ s) : addChapters newCs ts
	| otherwise = Header n s : addChapters newCs ts
	where
	chaps = concatMap (++ ".") $ map show $ catMaybes $ take n newCs
	newCs = take (n - 1) cs ++ [(+ 1) <$> cs !! (n - 1)] ++ map reset (drop n cs)
	reset (Just _) = Just 0
	reset _ = Nothing
addChapters cs (t : ts) = t : addChapters cs ts

preprocess :: [Text] -> [Text]
preprocess [] = []
preprocess (Paras strs : ts) = Paras (map remSpaces strs) : preprocess ts
preprocess (t : ts) = t : preprocess ts

remSpaces :: String -> String
remSpaces "" = ""
remSpaces (c : ' ' : c' : cs)
	| not (isAscii c) || not (isAscii c') = c : c' : remSpaces cs
remSpaces (c : cs) = c : remSpaces cs

getSize :: [(FilePath, String)] -> FilePath -> Size -> Maybe (Double, Double)
getSize dict fp sz = case lookup fp dict of
	Just src -> convertedSize src sz
	_ -> error $ show (map fst dict) ++ " " ++ show fp -- Nothing

getImageHeight :: [(FilePath, String)] -> Double -> Text -> Double
getImageHeight fp r (Image _ p ttl) = ht
	where
	ratio = width r - 2 * leftMargin r
	size = case ttl of
		"small" -> Small
		"large" -> Large
		_ -> Medium
	(wt, ht) = case getSize fp p size of
		Just (w_, h_) -> (w_ * ratio, h_ * ratio)
		_ -> (r * 1000, r * 1000)
getImageHeight _ _ _ = error "not Image"
