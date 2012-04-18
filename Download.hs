{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as CB
import Control.Monad
import Control.Monad.Trans
import qualified Data.CaseInsensitive as CI
import Control.Concurrent.Lifted
import System

--url = "http://ftp.unina.it/pub/linux/distributions/ubuntu-releases//oneiric/ubuntu-11.10-desktop-i386.iso"

url = "http://www.narutoplanet.it/wp-content/uploads/downloads/2012/04/naruto-capitolo-582.zip"

main :: IO ()
main = do
--    url <- fmap head getArgs
    withManager $ \manager -> do
        initReq <- parseUrl url
        let req = initReq { method = "HEAD" }
        Response _ _ hdrs bsrc <- http req manager
        let (Just size) = lookup (CI.mk "content-length") hdrs
        let cks = chunks (read . CB.unpack $ size) 4

        mapM_ (\(range,fp) -> download url fp range manager)
            $ zip cks [(show x) ++ ".data" | x <- [1..]]

        forever $ do
            threadDelay $ 1000 * 10

    return ()

mkReq url (s, f) = do
    initReq <- parseUrl url
    let headers = ((CI.mk "Range",
                    CB.pack $
                    "bytes=" ++ (show s) ++ "-" ++ (show f))) :
                  requestHeaders initReq
    return initReq { requestHeaders = headers }

download url fp range manager = do
    req <- mkReq url range
    Response _ _ _ bsrc <- http req manager
    fork $ bsrc C.$$ sinkFile fp

chunks n cn = cl n cs cn 0 0
  where
    cs = n `div` cn

cl n cs cn p t | (cn-1) == t = [(p, n)]
               | otherwise = (p, cs+p) : cl n cs cn (cs+p+1) (t+1)
