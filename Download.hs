{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Conduit
import Data.Conduit.Binary (sinkFile, conduitFile)
import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Data.Conduit as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as CB
import Control.Monad
import Control.Monad.Trans
import qualified Data.CaseInsensitive as CI
import Control.Concurrent (forkIO)
import Control.Concurrent.Lifted
import Control.Concurrent.SampleVar
import System
import System.IO

data Downloadable = DL
             { dlUrl :: String
             , dlFilename :: CB.ByteString
             , dlRanges :: Maybe [(String, String)]
             }
           deriving (Show, Eq)

main :: IO ()
main = do
    url <- fmap head getArgs
    withManager $ \manager -> do
        liftIO . putStrLn $ "Downloading " ++ url
        sinfo <- liftIO $ newSampleVar 0
        mvars <- download url 5 manager sinfo

        liftIO . forkIO . forever $ do
            rb <- readSampleVar sinfo
            if rb >= 14581350
                then liftIO $ putStr "#" >> hFlush stdout >>
                     (liftIO $ writeSampleVar sinfo 0)
                else liftIO $ writeSampleVar sinfo rb
            threadDelay 500

        mapM (liftIO . takeMVar) mvars
        liftIO $ putStrLn "Downloaded"

mkDownloadable url cn manager = do
    initReq <- parseUrl url
    let req = initReq { method = "HEAD" }
    Response _ _ hdrs _ <- http req manager
    let (Just size) = lookup (CI.mk "content-length") hdrs
    let ranges = mkRanges size cn
    let filename = last . CB.split '/' . path $ req
    return $ DL url filename ranges

mkRanges "-1" _ = Nothing
mkRanges size cn = return $ ranges (read . CB.unpack $ size) cn

mkReq url (s, f) = do
    initReq <- parseUrl url
    let respHdrs = requestHeaders initReq
    let headers = (,)
                  (CI.mk "Range")
                  (CB.pack $ "bytes=" ++ s ++ "-" ++ f) : respHdrs
    return $ initReq { requestHeaders = headers }

mkReqs (DL url _ Nothing) = return $ parseUrl url
mkReqs (DL url _ (Just ranges)) = mapM (mkReq url) ranges

download url cn manager sinfo = do
    dl@(DL url filename ranges) <- mkDownloadable url cn manager
    mvars <- liftIO $ replicateM cn newEmptyMVar
    let reqs = mkReqs dl
    mapM_ (\(req,fp,mvar) -> fork $ do
            Response _ _ _ bsrc <- http req manager
            bsrc C.$= (conduitInfo sinfo) C.$$ (sinkFile $ CB.unpack filename++fp)
            putMVar mvar True) $
        zip3 (join reqs)
        [".part" ++ (show x) | x <- [1..]]
        mvars
    return mvars

ranges n cn = cl n cs cn 0 0
  where
    cs = n `div` cn

conduitInfo :: MonadResource m
               => SampleVar Int -> Conduit B.ByteString m B.ByteString
conduitInfo sinfo = conduitIO
              (return ())
              (const $ return ())
              (\_ bs -> do
                    rb <- liftIO $ readSampleVar sinfo
                    let r = B.length bs
                    liftIO $ writeSampleVar sinfo (r + rb)
                    return $ IOProducing [bs])
              (const $ return [])


cl n cs cn p t | (cn-1) == t = [(sp, sn)]
               | otherwise = (sp, (show $ cs+p)) : cl n cs cn (cs+p+1) (t+1)
  where
    sp = (show p)
    sn = (show n)
