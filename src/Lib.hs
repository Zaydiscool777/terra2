{-# LANGUAGE TupleSections, BlockArguments, LexicalNegation #-}
module Lib where

import Raylib.Core as R
-- import Raylib.Core.Audio as Audio
-- import Raylib.Core.Models as Model
import Raylib.Core.Shapes as Shape
-- import Raylib.Core.Text as Text
-- import Raylib.Core.Textures as Texture

import Raylib.Types.Core
-- import Raylib.Types.Core.Audio
import Raylib.Types.Core.Camera
-- import Raylib.Types.Core.Models as Models
-- import Raylib.Types.Core.Text as Text
-- import Raylib.Types.Core.Textures as Textures

import Raylib.Util.Colors

import Linear
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Control.Lens

-- utils

-- untilM :: Monad m => (a -> Bool) -> m a -> m a from: utility-ht
loopUntil :: Monad m => (a -> Bool) -> (s -> m (s, a)) -> s -> m [a]
loopUntil p m i = go i where
  go s = do
    (s', x) <- m s
    if p x then return [] else (x :) <$> go s'

-- also :: (a -> Bool) -> (a -> State s a) -> a -> State s [a]
untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m [a]
untilM p m = loopUntil p \s -> (\x -> (x, x)) <$> m s
{-# INLINABLE untilM #-}

untilM' :: Monad m => (a -> Bool) -> m a -> m [a]
untilM' p m = loopUntil p (\_ -> ((),) <$> m) ()
{-# INLINABLE untilM' #-}

-- lesser utils

tilClose :: StateT Model IO () -> StateT Model IO ()
tilClose x = () <$ untilM' id (x >> liftIO windowShouldClose)

getKeysPressed :: IO [KeyboardKey]
getKeysPressed = untilM' (== KeyNull) getKeyPressed

getKeysDown :: IO [KeyboardKey]
getKeysDown = filterM isKeyDown [toEnum 0..]

-- elm

data Model = Model {
  player :: Vector2,
  vel :: Vector2
}

data Action =
  AStart
  | AEnd
  | AKey KeyboardKey

tick :: StateT Model IO ()
tick = do
  model <- get
  new <- liftIO (sub model)
  put new
  liftIO (view' model)

init' :: Model
init' = Model {
  player = pure 200,
  vel = pure 0
}

sub :: Model -> IO Model
sub i = do
  i1 <- upd i AStart
  x <- getKeysPressed
  i2 <- foldM upd i1 (AKey <$> x)
  upd i2 AEnd

movevel :: Float
movevel = 10

upd :: Model -> Action -> IO Model
upd m AStart = return m {vel = pure 0}
upd m (AKey KeyUp) = return m {vel = vel m & _y %~ (- movevel)}
upd m (AKey KeyDown) = return m {vel = vel m & _y %~ (+ movevel)}
upd m (AKey KeyLeft) = return m {vel = vel m & _x %~ (- movevel)}
upd m (AKey KeyRight) = return m {vel = vel m & _x %~ (+ movevel)}
upd m AEnd = return (move m)
upd m _ = return m

view' :: Model -> IO ()
view' m = do
  beginDrawing
  clearBackground rayWhite
  w <- getScreenWidth
  h <- getScreenHeight
  beginMode2D (center (player m) w h)
  drawtiles
  drawplayer (player m)
  endMode2D
  endDrawing -- presents frame to user

-- io

drawtiles :: IO ()
drawtiles = mapM_ (flip drawRectangleRec black) (tilesr 100)

drawplayer :: Vector2 -> IO ()
drawplayer v = drawCircleV v 10 red 

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

move :: Model -> Model
move m@Model {player = p, vel = v} = m {player = p + v}
