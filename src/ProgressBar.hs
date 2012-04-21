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

module ProgressBar
       ( mkProgressBar
       ) where

import Text.Printf

mkProgressBar :: Int -> Int -> Int -> String
mkProgressBar width filesize rbytes =
    printf "%d%%  [%s%s] %s   " percentage bar spaces sbytes
  where
    bar = replicate completed '#'
    spaces = replicate (width - completed) ' '
    percentage = completed * 100 `div` width
    sbytes = (bytesConv . fromIntegral $ rbytes)
    completed = if rbytes /= filesize
                then rbytes * width `div` filesize
                else width

fixNum num = show $ fromInteger (truncate (num * 100)) / 100

bytesConv :: Double -> String
bytesConv b | b >= 1073741824 = fixNum (b / 1073741824) ++ " GB"
            | b >= 1048576 = fixNum (b / 1048576) ++ " MB"
            | b >= 1024 = fixNum (b / 1024) ++ " KB"
            | otherwise = fixNum b ++ " B"
