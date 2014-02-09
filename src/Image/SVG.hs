{-# LANGUAGE QuasiQuotes #-}

module Image.SVG (isSVG, svgSize) where

import Control.Applicative
import Text.Papillon
import Data.Char

doctype0 :: DocType
doctype0 = DocType "svg" "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"

xmlns0 :: String
xmlns0 = "http://www.w3.org/2000/svg"

isSVG :: String -> Bool
isSVG svg = case getSVGHeader svg of
	Just (_, d, SVG s) -> d == doctype0 && isValueOf xmlns0 s "xmlns"
	_ -> False

svgSize :: String -> Maybe (Double, Double)
svgSize svg = case getSVGHeader svg of
	Just (_, _, SVG s) ->
		(,) <$> (read <$> lookup "width" s) <*> (read <$> lookup "height" s)
	_ -> Nothing

isValueOf :: String -> [(String, String)] -> String -> Bool
isValueOf v0 dict key = case lookup key dict of
	Just v -> v == v0
	_ -> False

getSVGHeader :: String -> Maybe (Header, DocType, SVG)
getSVGHeader src = case runError $ svgHeader $ parse src of
	Right (sh, _) -> Just sh
	_ -> Nothing

getHeader :: String -> Maybe Header
getHeader src = case runError $ header $ parse src of
	Right (h, _) -> Just h
	_ -> Nothing

getComment :: String -> Maybe String
getComment src = case runError $ comment $ parse src of
	Right (c, _) -> Just c
	_ -> Nothing

getDocType :: String -> Maybe DocType
getDocType src = case runError $ doctype $ parse src of
	Right (d, _) -> Just d
	_ -> Nothing

data Header = Header [(String, String)] deriving Show

data DocType = DocType String String String deriving (Eq, Show)

data SVG = SVG [(String, String)] deriving Show

[papillon|

svgHeader :: (Header, DocType, SVG)
	= h:header _:spccmm d:doctype _:spccmm s:svg
	{ (h, d, s) }
--	= h:header _:spccmm d:doctype _:spccmm s:svg
--	{ (h, d, s) }

header :: Header
	= '<' '?' 'x' 'm' 'l' _:space+ vss:(vs:varstr _:space* { vs })*
		'?' '>'
	{ Header vss }

varstr :: (String, String)
	= v:name '=' s:string		{ (v, s) }

name :: String
	= v:variable ':' n:name		{ v ++ ":" ++ n }
	/ v:variable			{ v }

variable :: String
	= v:<isLower>+			{ v }

string :: String
	= '\'' s:<(/= '\'')>+ '\''	{ s }
	/ '"' s:<(/= '"')>+ '"'		{ s }

spccmm :: () = _:(_:space / _:comment)*

space :: ()
	= _:<isSpace>

comment :: String
	= '<' '!' '-' '-' cm:(!_:('-' '-' '>') c { c })* '-' '-' '>'
					{ cm }

doctype :: DocType
	= '<' '!' 'D' 'O' 'C' 'T' 'Y' 'P' 'E' _:space+ v:variable _:space+
		'P' 'U' 'B' 'L' 'I' 'C' _:space* s1:string _:space*
		s2:string _:space* '>'
		{ DocType v s1 s2 }

svg :: SVG
	= '<' 's' 'v' 'g' _:space+ vss:(vs:varstr _:space* { vs })* '>'
	{ SVG vss }

|]
