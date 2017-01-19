module Main (main)
where

import System.IO
import Image


{-
This is neither an efficient way of displaying graphics nor an efficient program for doing that -
drawing a 1000x1000 fractal for a more complicated function might take an hour or more, so for efficiency
draw smaller pictures first to see if that's what you are looking for, then do it once more with a bigger picture
(and think of something productive while you are waiting for the picture to be drawn)
-}

main = do
	writeFile "file.ppm" ("P3\n" ++ show dimension ++ " " ++ show dimension ++ "\n" ++ "255\n" ++ display)
