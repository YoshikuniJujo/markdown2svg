import Control.Applicative
import Control.Monad
import System.Environment
import System.FilePath
import System.Console.GetOpt
import System.Exit

import Text.Markdown.Pap
import SVG

main :: IO ()
main = do
	(opts, [fp], emsgs) <- getOpt Permute options <$> getArgs
	mapM_ putStrLn emsgs
	when (not $ null emsgs) exitFailure
	let	size = getSizeR opts
		dir = getDir opts
	cnt <- readFile fp
	case parse cnt of
		Just t -> forM_ (zip [1 ..] $ textToSVG True size t) $ \(i, s) ->
			writeFile (mkSVGFileName dir fp i) s
		_ -> return ()

mkSVGFileName :: Maybe FilePath -> FilePath -> Int -> FilePath
mkSVGFileName (Just dir) fp i = dir </> (takeBaseName fp ++ show2 i) <.> "svg"
mkSVGFileName _ fp i = (dropExtension fp ++ show2 i) <.> "svg"

show2 :: Show a => a -> String
show2 x = replicate (2 - length s) '0' ++ s
	where
	s = show x

data Option = Dir FilePath | Size Double deriving Show

options :: [OptDescr Option]
options = [
	Option "d" [] (ReqArg Dir "output directory") "set ouput directory",
	Option "s" [] (ReqArg (Size . read) "size ratio") "set size ratio" ]

getDir :: [Option] -> Maybe FilePath
getDir [] = Nothing
getDir (Dir d : _) = Just d
getDir (_ : ops) = getDir ops

getSizeR :: [Option] -> Double
getSizeR [] = 0.15
getSizeR (Size sr : _) = sr
getSizeR (_ : ops) = getSizeR ops
