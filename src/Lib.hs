module Lib where

import Raylib.Core as R
import Raylib.Core.Audio as Audio
-- import Raylib.Core.Models as Model
import Raylib.Core.Shapes as Shape
import Raylib.Core.Text as Text
import Raylib.Core.Textures as Texture

import Raylib.Types.Core
import Raylib.Types.Core.Audio
import Raylib.Types.Core.Camera
-- import Raylib.Types.Core.Models as Models
import Raylib.Types.Core.Text as Text
import Raylib.Types.Core.Textures as Textures

import Raylib.Util.Colors

import Linear
import Control.Monad
import Control.Applicative
import Data.Maybe

-- utils

-- untilM :: Monad m => (a -> Bool) -> m a -> m a from: utility-ht
untilM :: Monad m => (a -> Bool) -> m a -> m [a]
untilM p m =
  let go =
        do
          x <- m
          if p x
            then return []
            else (x :) <$> go
    in go

tilClose :: IO () -> IO ()
tilClose x = untilM id (x >> windowShouldClose) >> return ()

getKeysPressed :: IO [KeyboardKey]
getKeysPressed = untilM (== KeyNull) getKeyPressed

-- io

tick :: IO ()
tick = do
  beginDrawing
  clearBackground rayWhite
  beginMode2D (Camera2D (V2 0 0) (V2 0 0) 0 1)
  drawtiles
  endMode2D
  endDrawing -- presents frame to user

drawtiles :: IO ()
drawtiles = mapM_ (flip drawRectangleRec black) (tilesr 100)

-- pure

tiles :: [[Integer]]
tiles = [
  [1, 1, 1, 1, 1],
  [1, 0, 0, 0, 1],
  [1, 0, 0, 0, 1],
  [1, 0, 0, 0, 1],
  [1, 1, 1, 1, 1]]

tilesr :: Float -> [Rectangle]
tilesr k = (\(a, b) -> Rectangle (a * k) (b * k) k k) <$> (m >>= catMaybes)
  where m = zipWith (\i r -> zipWith (\j x -> guard (x == 1) >> Just (i, j)) [0..] r) [0..] tiles

center :: Vector2 -> Int -> Int -> Camera2D
center v c d = Camera2D ((/ 2) . fromIntegral <$> V2 c d) v 0 1

move :: Vector2 -> [KeyboardKey] -> Int -> [Rectangle] -> Vector2
move pos keys airtime hitboxes = undefined
