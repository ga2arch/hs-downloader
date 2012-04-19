{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import Network.HTTP.Types
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

data Downloadable = DL
             { dlUrl :: String
             , dlFilename :: CB.ByteString
             , dlRanges :: Maybe [(String, String)]
             }
           deriving (Show, Eq)

mkDownloadable url cn manager = do
    initReq <- parseUrl url
    let req = initReq { method = "HEAD" }
    Response _ _ hdrs _ <- http req manager
    let (Just size) = lookup (CI.mk "content-length") hdrs
    let ranges = mkRanges size cn
    let filename = last . CB.split '/' . path $ req
    return $ DL url filename ranges

mkRanges "-1" _ = Nothing
mkRanges size cn = return $ chunks (read . CB.unpack $ size) cn

main :: IO ()
main = do
--    url <- fmap head getArgs
    withManager $ \manager -> do
        download url 4 manager

        forever $ do
            threadDelay $ 1000 * 10

mkReq url (s, f) = do
    initReq <- parseUrl url
    let respHdrs = requestHeaders initReq
    let headers = (,)
                  (CI.mk "Range")
                  (CB.pack $ "bytes=" ++ s ++ "-" ++ f) : respHdrs
    return $ initReq { requestHeaders = headers }

mkReqs (DL url _ Nothing) = return $ parseUrl url
mkReqs (DL url _ (Just ranges)) = mapM (mkReq url) ranges

download url cn manager = do
    dl@(DL url filename ranges) <- mkDownloadable url cn manager
    let reqs = mkReqs dl
    mapM_ (\(req,fp) -> fork $ do
            Response _ _ _ bsrc <- http req manager
            bsrc C.$$ sinkFile $ CB.unpack filename++fp) $
        zip (join reqs) [".part" ++ (show x) | x <- [1..]]

chunks n cn = cl n cs cn 0 0
  where
    cs = n `div` cn

cl n cs cn p t | (cn-1) == t = [((show p), (show n))]
               | otherwise = ((show p), (show $ cs+p)) : cl n cs cn (cs+p+1) (t+1)
