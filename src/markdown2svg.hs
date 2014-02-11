import Control.Applicative
import Control.Monad
import Data.Maybe
import System.Environment
import System.FilePath
import System.Console.GetOpt
import System.Exit
import System.Directory
import File.Binary

import Text.Markdown.Pap (parse)
import SVG

main :: IO ()
main = do
	(opts, [fp], emsgs) <- getOpt Permute options <$> getArgs
	mapM_ putStrLn emsgs
	when (not $ null emsgs) exitFailure
	let	size = getSizeR opts
		dir = getDir opts
		idir = getIDir opts
		hfont = getFont opts Header
		nfont = getFont opts Normal
		cfont = getFont opts Code
	cnt <- readFile fp
	is <- mapM pathCont =<< getImageFilePath (fromMaybe "." idir)
	copy (fromMaybe "." idir) (fromMaybe "." dir) $ map (takeFileName . fst) is
	case parse cnt of
		Just t -> forM_ (zip [1 ..] $ textToSVG (hfont, nfont, cfont) is True size t) $ \(i, s) ->
			writeFile (mkSVGFileName dir fp i) s
		_ -> return ()

mkSVGFileName :: Maybe FilePath -> FilePath -> Int -> FilePath
mkSVGFileName (Just dir) fp i = dir </> (takeBaseName fp ++ show2 i) <.> "svg"
mkSVGFileName _ fp i = (dropExtension fp ++ show2 i) <.> "svg"

show2 :: Show a => a -> String
show2 x = replicate (2 - length s) '0' ++ s
	where
	s = show x

data Option
	= Dir FilePath
	| Size Double
	| ImageDir FilePath
	| Font FontType String
	deriving Show

data FontType = Header | Normal | Code deriving (Eq, Show)

options :: [OptDescr Option]
options = [
	Option "d" [] (ReqArg Dir "output directory") "set ouput directory",
	Option "s" [] (ReqArg (Size . read) "size ratio") "set size ratio",
	Option "i" [] (ReqArg ImageDir "image directory") "set image directory",
	Option "" ["header-font"] (ReqArg (Font Header) "header font") "set header font",
	Option "" ["normal-font"] (ReqArg (Font Normal) "normal font") "set normal font",
	Option "" ["code-font"] (ReqArg (Font Code) "code font") "set code font" ]

getDir, getIDir :: [Option] -> Maybe FilePath
getDir [] = Nothing
getDir (Dir d : _) = Just d
getDir (_ : ops) = getDir ops

getIDir [] = Nothing
getIDir (ImageDir d : _) = Just d
getIDir (_ : ops) = getIDir ops

getSizeR :: [Option] -> Double
getSizeR [] = 0.15
getSizeR (Size sr : _) = sr
getSizeR (_ : ops) = getSizeR ops

getFont :: [Option] -> FontType -> String
getFont [] Header = "Sans"
getFont [] Normal = "Serif"
getFont [] Code = "Monospace"
getFont (Font ft fn : _) ft0
	| ft == ft0 = fn
getFont (_ : ops) ft0 = getFont ops ft0

getImageFilePath :: FilePath -> IO [FilePath]
getImageFilePath fp = do
	fps <- getDirectoryContents fp
	return $ map (fp </>) $
		filter ((`elem` [".jpg", ".png", ".svg"]) . takeExtension) fps

pathCont :: FilePath -> IO (FilePath, String)
pathCont fp = do
	cnt <- readBinaryFile fp
	print $ takeFileName fp
	return (takeFileName fp, cnt)

copy :: FilePath -> FilePath -> [FilePath] -> IO ()
copy s d f = zipWithM_ copyFile (map (s </>) f) (map (d </>) f)
