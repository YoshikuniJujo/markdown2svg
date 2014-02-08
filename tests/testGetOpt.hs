import System.Environment
import System.Console.GetOpt

main :: IO ()
main = do
	args <- getArgs
	print $ getOpt Permute options args

data Option = Dir FilePath | Size Double deriving Show

options :: [OptDescr Option]
options = [
	Option "d" [] (ReqArg Dir "output directory") "set ouput directory",
	Option "s" [] (ReqArg (Size . read) "size ratio") "set size ratio" ]

getDir :: [Option] -> FilePath
getDir [] = "./"
getDir (Dir d : _) = d
getDir (_ : ops) = getDir ops

getSizeR :: [Option] -> FilePath
getSizeR [] = 0.15
getSizeR (Size sr : _) = sr
getSizeR (_ : ops) = getSizeR ops
