{-
Copyright (c) 2011 Gabriele Carrettoni <gabriele.carrettoni@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}


{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Download
       (downloadUrl
       ) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent (forkIO)
import Control.Concurrent.Lifted
import Control.Concurrent.Chan
import Network.HTTP.Conduit
import Network.HTTP.Types
import System.IO
import System.Directory
import Text.Printf
import Data.Maybe

import qualified Data.Conduit as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as CB
import qualified Data.CaseInsensitive as CI

import Types
import ProgressBar

downloadUrl url = withManager $ \manager -> do
    dl <- mkDownloadable url 5 manager
    liftIO $ putStrLn ("Downloading " ++ url)
    chan <- liftIO newChan
    download dl chan manager
    h <- liftIO $ openFile (CB.unpack $ dlFilename dl) WriteMode
    liftIO $ do
        hSetBuffering h NoBuffering
        putStr $ mkProgressBar 40 (dlSize dl) 0 ++ "\r"
        fileWriter h chan dl 0 0
    return ()

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

cl :: Int -> Int -> Int -> Int -> Int -> [(String, String)]
cl n cs cn p t | (cn-1) == t = [(sp, sn)]
               | otherwise = (sp, show $ cs+p) : cl n cs cn (cs+p+1) (t+1)
  where
    sp = show p
    sn = show n

sinkFile filesize r chan =
    C.sinkIO
    (newMVar r)
    (const $ return ())
    (\m bs -> do
          rb <- liftIO $ takeMVar m
          let nrb = rb + CB.length bs
          liftIO $ writeChan chan (toInteger rb, bs)
          if nrb /= filesize
              then liftIO (putMVar m nrb)
                   >> return C.IOProcessing
              else return $ C.IODone Nothing ())
    (const $ return ())

download dl chan manager = do
    reqs <- mkReqs dl
    mapM_ (\(req,fp,(r,_)) -> fork $ do
            Response _ _ _ bsrc <- http req manager
            bsrc C.$$ sinkFile (dlSize dl) (read r) chan) $
        zip3 reqs
        [".part" ++ show x | x <- [1..]]
        (fromJust $ dlRanges dl)

fileWriter h chan dl@(DL{..}) total rbytes = do
    (pos, bs) <- readChan chan

    hSeek h AbsoluteSeek pos
    CB.hPut h bs

    let nbytes = CB.length bs
    let ntotal = total + nbytes
    let nrbytes = if rbytes >= (dlSize `div` 40) then 0 else rbytes + nbytes
    let ppBar as = putStr $ mkProgressBar 40 dlSize ntotal ++ as

    when (nrbytes == 0) $
        ppBar "\r"

    if ntotal == dlSize
        then ppBar "\n"
             >> hClose h
             >> return ()
        else fileWriter h chan dl ntotal nrbytes
