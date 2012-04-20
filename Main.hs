import Download
import System.Environment
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    url <- fmap head getArgs
    downloadUrl url
