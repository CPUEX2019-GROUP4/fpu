import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.FilePath.Posix
import AsmConv.Util
import AsmConv.Run

data Options = Options {
    optOutput :: Maybe FilePath,
    optInput :: Maybe FilePath,
    optConfig :: FilePath
}

defaultOptions :: Options
defaultOptions = Options {
    optOutput = Nothing,
    optInput = Nothing,
    optConfig = "config"
}

main :: IO ()
main = do
    Options { optOutput = Just foutput,
        optInput = Just finput,
        optConfig = fconfig } <- parseOpts <$> getArgs >>= flip eitherToIO id
    config <- readFile fconfig
    input <- readFile finput
    output <- flip eitherToIO id $ run config input
    writeFile foutput output

parseOpts :: [String] -> Either String Options
parseOpts argv =
    case getOpt Permute options argv of
        (o, [i], []) ->
            let opts = foldl (flip id) defaultOptions o
                opts' = opts {
                    optInput = Just i,
                    optOutput = Just $ fromMaybe
                        (replaceExtensions i "p.s")
                        (optOutput opts)
                }
            in Right opts'
        (_, _, []) -> Left $ "need one input file\n" ++ usageInfo header options
        (_, _, e) -> Left $ concat e ++ usageInfo header options
    where
        header = "Usage: pas [OPTION...] file"

options :: [OptDescr (Options -> Options)]
options = [
    Option ['o'] ["output"]
        (ReqArg (\ f opts -> opts { optOutput = Just f }) "FILE")
        "output FILE; default: INPUT_FILE_NAME.p.s",
    Option ['c'] ["config"]
        (ReqArg (\ f opts -> opts { optConfig = f }) "FILE")
        "config FILE; default: config"
    ]
