{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Data.Conduit as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as CB
import Control.Monad
import Control.Monad.Trans
import qualified Data.CaseInsensitive as CI
import Control.Concurrent (forkIO)
import Control.Concurrent.Lifted
import Control.Concurrent.Chan
import System
import System.IO
import System.Directory
import Text.Printf
import Data.Maybe

data Downloadable = DL
             { dlUrl :: String
             , dlFilename :: CB.ByteString
             , dlSize :: Int
             , dlRanges :: Maybe [(String, String)]
             }
           deriving (Show, Eq)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    url <- fmap head getArgs
    withManager $ \manager -> do
        dl <- mkDownloadable url 5 manager
        liftIO . putStrLn $ "Downloading " ++ url
        sinfo <- liftIO $ newMVar 0

        chan <- liftIO $ newChan
        download dl chan manager sinfo
        h <- liftIO $ openFile (CB.unpack $ dlFilename dl) WriteMode
        liftIO $ hSetBuffering h NoBuffering
        liftIO $ fileWriter h chan dl 0 0
        liftIO $ putStrLn "\nDownloaded"
        return ()

mkProgressBar :: String -> Int -> Int -> Int -> String
mkProgressBar msg width filesize rbytes =
    printf "%s [%s%s] %d%%" msg bar spaces percentage
  where
    bar = replicate completed '#'
    spaces = replicate (width - completed) ' '
    percentage = completed * 100 `div` width
    completed = if rbytes /= filesize
                then rbytes * width `div` filesize
                else width

mkDownloadable url cn manager = do
    initReq <- parseUrl url
    let req = initReq { method = "HEAD" }
    Response _ _ hdrs _ <- http req manager
    let (Just size) = lookup (CI.mk "content-length") hdrs
    let ranges = mkRanges size cn
    let filename = last . CB.split '/' . path $ req
    return $ DL url filename (read $ CB.unpack size) ranges

mkRanges :: CB.ByteString -> Int -> Maybe [(String, String)]
mkRanges "-1" _ = Nothing
mkRanges size cn = return $ ranges (read . CB.unpack $ size) cn

mkReq url (s, f) = do
    initReq <- parseUrl url
    let respHdrs = requestHeaders initReq
    let headers = (,)
                  (CI.mk "Range")
                  (CB.pack $ "bytes=" ++ s ++ "-" ++ f) : respHdrs
    return $ initReq { requestHeaders = headers }

mkReqs (DL url _ _ Nothing) = return $ parseUrl url
mkReqs (DL url _ _ (Just ranges)) = mapM (mkReq url) ranges

ranges :: Int -> Int -> [(String, String)]
ranges n cn = cl n cs cn 0 0
  where
    cs = n `div` cn

conduitInfo :: C.MonadResource m
               => MVar Int -> C.Conduit B.ByteString m B.ByteString
conduitInfo env = C.conduitIO
              (return ())
              (const $ return ())
              (\_ bs -> do
                    liftIO . modifyMVar_ env $ \rb -> do
                        return $ rb + (CB.length bs)
                    return $ C.IOProducing [bs])
              (const $ return [])

cl :: Int -> Int -> Int -> Int -> Int -> [(String, String)]
cl n cs cn p t | (cn-1) == t = [(sp, sn)]
               | otherwise = (sp, (show $ cs+p)) : cl n cs cn (cs+p+1) (t+1)
  where
    sp = (show p)
    sn = (show n)

sinkFile filesize r chan = C.sinkIO
              (newMVar r)
              (const $ return ())
              (\m bs -> do
                    rb <- liftIO $ takeMVar m
                    let nrb = rb + (CB.length bs)
                    liftIO $ writeChan chan (toInteger rb, bs)
                    if nrb /= filesize
                        then (liftIO $ putMVar m nrb)
                             >> return C.IOProcessing
                        else return $ C.IODone Nothing ())
              (const $ return ())

download dl chan manager env = do
    reqs <- mkReqs dl
    mapM_ (\(req,fp,(r,_)) -> fork $ do
            Response _ _ _ bsrc <- http req manager
            bsrc C.$$ sinkFile (dlSize dl) (read r) chan) $
        zip3 reqs
        [".part" ++ (show x) | x <- [1..]]
        (fromJust $ dlRanges dl)

fileWriter h chan dl@(DL _ _ fsize _) total rbytes = do
    (pos, bs) <- readChan chan
    hSeek h AbsoluteSeek pos
    CB.hPut h bs

    let nbytes = CB.length bs
    let ntotal = total + nbytes

    let nrbytes = if rbytes >= (fsize `div` 40)
                  then 0
                  else rbytes + nbytes

    if nrbytes == 0
        then putStr $ mkProgressBar "Downloading" 40 fsize ntotal ++ "\r"
        else return ()

    if ntotal == fsize
        then (putStr $ mkProgressBar "Downloading" 40 fsize ntotal ++ "\r")
             >> (hClose h) >> return ()
        else fileWriter h chan dl ntotal nrbytes
