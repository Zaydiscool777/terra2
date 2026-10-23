module Main (main) where

import Raylib.Core as R
import Raylib.Core.Text as Text
import Raylib.Types.Core.Camera
import Raylib.Util.Colors

import Linear
import Control.Monad
import Lib

main :: IO ()
main = do
  window <- initWindow 1000 1000 "hello"
  tilClose tick
  closeWindow (Just window)
