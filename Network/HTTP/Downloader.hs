{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, RecordWildCards, FlexibleContexts #-}

module Downloader where

import Data.String.Utils
import Control.Exception
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Network (withSocketsDo)
import Network.HTTP.Conduit
import System.IO

import qualified Data.Conduit as C
import qualified Data.Conduit.Util as CU
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

import Prelude hiding (catch)

url = "http://www.google.it/index.html"
url2 = "http://cdimage.ubuntu.com/daily-live/current/quantal-desktop-i386.iso"

main :: IO ()
main = withSocketsDo $ download url

download :: String -> IO ()
download url = do
    let req = parseUrl url :: Either HttpException (Request m)
    case req of
        Left  ex  -> print "Invalid Url"
        Right req -> do
            (dl url req) `catches`
                     [ Handler (\(e :: IOException) -> putStrLn $ show e)
                     , Handler handleHttpException]

dl url req = withManager $ \manager -> do
    r@(Response status _ headers body) <- http req manager
    let cl = lookup (CI.mk "content-length") headers
    case cl of
        Just size -> return $ process url (read $ B.unpack size :: Int) r manager
        Nothing   -> return $ process url 0 r manager

    return ()

process url size Response{..} m = do
    mvar <- newEmptyMVar
    if size > 0
        then mapM_ (\range@(s, f) ->
               liftIO . forkIO . C.runResourceT $ do
                   req <- mkReq url range
                   Response _ _ _ src <- http req m
                   src C.$$+- sinkFile (read s) size mvar) (ranges size 4)
        else do
        liftIO . forkIO . C.runResourceT $ responseBody C.$$+- sinkFile 0 size mvar
        return ()

    bracket
        (openFile fname WriteMode)
        (hClose)
        (\h -> fileWriter h mvar)

  where
    fname = last . split "/" $ url

handleHttpException :: HttpException -> IO ()
handleHttpException (StatusCodeException _ _) = putStrLn "404"

mkReq url (s, f) = do
    initReq <- parseUrl url
    let respHdrs = requestHeaders initReq
    let headers = (,)
                  (CI.mk "Range")
                  (B.pack $ "bytes=" ++ s ++ "-" ++ f) : respHdrs
    return $ initReq { requestHeaders = headers }

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

sinkFile start size mvar =
    CU.sinkIO
    (newMVar start)
    (const $ return ())
    (\mpos bs -> do
          current <- liftIO $ modifyMVar mpos
                     (\pos -> return (pos, (pos + B.length bs)))
          if current == size
              then return $ CU.IODone Nothing ()
              else liftIO $ do
              d <- takeMVar mvar
              putMVar mvar $ (current, bs):d
              return CU.IOProcessing)
    (const $ return ())

fileWriter handle mvar = do
    buffer <- takeMVar mvar
    if length buffer > 3
        then putMVar mvar []
        else putMVar mvar buffer
    mapM_ write buffer
    fileWriter handle mvar
  where
    write (pos, bs) = do
        hSeek handle AbsoluteSeek (fromIntegral pos)
        B.hPut handle bs
