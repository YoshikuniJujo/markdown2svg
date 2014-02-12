module SepWords (
	splitWords,
	sepWords
) where

import Data.Char

splitWords :: Int -> String -> (String, String)
splitWords = flip splitWords_ 0

splitWords_ :: Int -> Int -> String -> (String, String)
splitWords_ _ _ "" = ("", "")
splitWords_ n k (c : c'@('?') : cs)
	| k > n = ([c, c'], cs)
	| otherwise = let (l, ls) = splitWords_ n (k + 4) cs in (c : c' : l, ls)
splitWords_ n k (c : c'@('」') : cs)
	| k > n = ([c, c'], cs)
	| otherwise = let (l, ls) = splitWords_ n (k + 4) cs in (c : c' : l, ls)
splitWords_ n k (c : c'@('、') : cs)
	| k > n = ([c, c'], cs)
	| otherwise = let (l, ls) = splitWords_ n (k + 4) cs in (c : c' : l, ls)
splitWords_ n k (c : c'@('。') : cs)
	| k > n = ([c, c'], cs)
	| otherwise = let (l, ls) = splitWords_ n (k + 4) cs in (c : c' : l, ls)
splitWords_ n k ca@(c@('「') : cs)
	| k > n = ("", ca)
	| otherwise = let (l, ls) = splitWords_ n (k + 2) cs in (c : l, ls)
splitWords_ n k (c : cs)
	| isSpace c && k > n = ("", cs)
	| not (isAscii c) && k > n = ([c], cs)
	| not $ isAscii c = let (l, ls) = splitWords_ n (k + 2) cs in (c : l, ls)
	| otherwise = let (l, ls) = splitWords_ n (k + 1) cs in (c : l, ls)

sepWords :: Int -> String -> [String]
sepWords = flip sepWords_ 0

sepWords_ :: Int -> Int -> String -> [String]
sepWords_ _ _ "" = [""]
sepWords_ n k (c : c'@('?') : cs)
	| k > n = [c, c'] : sepWords_ n 0 cs
	| otherwise = let l : ls = sepWords_ n (k + 4) cs in (c : c' : l) : ls
sepWords_ n k (c : c'@('」') : cs)
	| k > n = [c, c'] : sepWords_ n 0 cs
	| otherwise = let l : ls = sepWords_ n (k + 4) cs in (c : c' : l) : ls
sepWords_ n k (c : c'@('、') : cs)
	| k > n = [c, c'] : sepWords_ n 0 cs
	| otherwise = let l : ls = sepWords_ n (k + 4) cs in (c : c' : l) : ls
sepWords_ n k (c : c'@('。') : cs)
	| k > n = [c, c'] : sepWords_ n 0 cs
	| otherwise = let l : ls = sepWords_ n (k + 4) cs in (c : c' : l) : ls
sepWords_ n k (c@('「') : cs)
	| k > n = let l : ls = sepWords_ n 2 cs in "" : (c : l) : ls
	| otherwise = let l : ls = sepWords_ n (k + 2) cs in (c : l) : ls
sepWords_ n k (c : cs)
	| isSpace c && k > n = "" : sepWords_ n 0 cs
	| not (isAscii c) && k > n = [c] : sepWords_ n 0 cs
	| not $ isAscii c = let l : ls = sepWords_ n (k + 2) cs in (c : l) : ls
	| otherwise = let l : ls = sepWords_ n (k + 1) cs in (c : l) : ls
