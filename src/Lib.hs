{-# LANGUAGE TupleSections, BlockArguments, LexicalNegation #-}
module Lib ( init', tick, tilClose ) where

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
loopUntil :: Monad m => (a -> Bool) -> StateT s m a -> s -> m [a]
loopUntil p m s = do
  (x, s') <- runStateT m s
  if p x then return [] else (x :) <$> loopUntil p m s'

-- also :: (a -> Bool) -> (a -> State s a) -> a -> State s [a]
-- untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m [a]
-- untilM p m = loopUntil p (get >>= lift . m)

untilM' :: Monad m => (a -> Bool) -> m a -> m [a]
untilM' p m = loopUntil p (lift m) ()

-- lesser utils

tilClose :: StateT Model IO () -> StateT Model IO ()
tilClose x = void $ untilM' id (x >> liftIO windowShouldClose)

-- getKeysPressed :: IO [KeyboardKey]
-- getKeysPressed = untilM' (== KeyNull) getKeyPressed

keysint :: [Int]
keysint = join [[0,4,5,24,25,32,39,59,61,96],[44..57],[65..93],[256..269],[280..284],[290..301],[320..336],[340..348]]

getKeysDown :: IO [KeyboardKey]
getKeysDown = filterM isKeyDown (toEnum <$> keysint)

-- elm

data Model = Model {
  player :: Vector2,
  uvel :: Vector2,
  gvel :: Float
} deriving Show

init' :: Model
init' = Model {
  player = pure 200,
  uvel = pure 0,
  gvel = 0
}

tick :: StateT Model IO ()
tick = do
  model <- get
  new <- liftIO (sub model)
  put new
  liftIO (view' model)

data Action =
  AStart
  | AEnd
  | AKey KeyboardKey
  deriving Show

sub :: Model -> IO Model
sub i = do
  i1 <- upd i AStart
  x <- getKeysDown
  i2 <- foldM upd i1 (AKey <$> x)
  upd i2 AEnd

upd :: Model -> Action -> IO Model
upd m AStart = return m {uvel = pure 0}
upd m (AKey KeyUp) = return m {uvel = uvel m & _y %~ (- 10)}
upd m (AKey KeyDown) = return m {uvel = uvel m & _y %~ (+ 10)}
upd m (AKey KeyLeft) = return m {uvel = uvel m & _x %~ (- 10)}
upd m (AKey KeyRight) = return m {uvel = uvel m & _x %~ (+ 10)}
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
  print m
  waitTime 0.1

-- io

drawtiles :: IO ()
drawtiles = mapM_ (`drawRectangleRec` black) tilesr

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

tilesr' :: Float -> [Rectangle]
tilesr' k = (\(a, b) -> Rectangle (a * k) (b * k) k k) <$> (m >>= catMaybes)
  where m = zipWith (\i r -> zipWith (\j x -> guard (x == 1) >> Just (i, j)) [0..] r) [0..] tiles

tilesr :: [Rectangle]
tilesr = [Rectangle 100 100 400 125] -- tilesr' 100

center :: Vector2 -> Int -> Int -> Camera2D
center v c d = Camera2D ((/ 2) . fromIntegral <$> V2 c d) v 0 1

move :: Model -> Model
move m@Model {player = p, uvel = v, gvel = g} =
  m {player = p', gvel = g'}
  where
    (p', g') = case foldr (block . (p,)) (Left (g + v^._y + 1)) tilesr of
      Left x -> (p + v + V2 0 (g - v^._y), x)
      Right x -> (x + v, 0)

block :: (Vector2, Rectangle) -> Either Float Vector2 -> Either Float Vector2 -- foldable collision
block _ x@(Right _) = x
block (V2 px py, Rectangle rx ry rw _) (Left pg) =
  if (compare px rx, compare px (rx + rw)) == (GT, LT)
  then case (compare py ry, compare (py + pg) ry) of
    (LT, LT) -> Left pg
    _ -> Right (V2 px ry)
  else Left pg
