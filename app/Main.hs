module Main (main) where

import Raylib.Core as R
import Raylib.Core.Text as Text
import Raylib.Types.Core.Camera
import Raylib.Util.Colors

import Linear
import Lib

main :: IO ()
main = do
  window <- initWindow 1000 1000 "hello"
  beginDrawing
  clearBackground rayWhite
  beginMode2D (Camera2D (V2 0 0) (V2 0 0) 0 1)
  drawtiles
  endMode2D
  endDrawing -- presents frame to user
  waitTime 4
  closeWindow (Just window)
