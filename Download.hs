{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit.Binary (sinkFile, conduitFile)
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
import System
import System.IO
import System.Directory
import Text.Printf

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
        download dl manager sinfo
        liftIO $ progress 40 sinfo (dlSize dl)
        liftIO $ putStrLn "\nDownloaded"

        let filename = CB.unpack $ dlFilename dl
        liftIO $ filesJoin filename 5
        return ()

progress :: Int -> MVar Int -> Int -> IO ()
progress width sinfo size = do
    rb <- readMVar sinfo
    putStr $ mkProgressBar "Downloading" width size rb ++ "\r"
    if rb /= size
        then threadDelay 500 >> progress width sinfo size
        else return ()

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

download dl manager env = do
    reqs <- mkReqs dl
    mapM_ (\(req,fp) -> fork $ do
            Response _ _ _ bsrc <- http req manager
            bsrc C.$= (conduitInfo env)
                 C.$$ (sinkFile $ CB.unpack (dlFilename dl)++fp)) $
        zip reqs
        [".part" ++ (show x) | x <- [1..]]

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


filesJoin :: String -> Int -> IO ()
filesJoin filename partsNum = do
    files <- sequence . map BL.readFile $ parts
    BL.writeFile filename $ BL.concat files
    mapM_ removeFile parts
  where
    parts = [filename ++ ".part" ++ (show x) | x <- [1..partsNum]]
