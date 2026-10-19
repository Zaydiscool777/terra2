module Lib (init', upd, subs, view') where

import Helm as H
import qualified Helm.Asset as H.Ast
import qualified Helm.Cmd as H.Cmd
import qualified Helm.Color as H.Color
import qualified Helm.Engine as H.Eng
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Engine.SDL.Asset as SDL.Ast
import qualified Helm.Engine.SDL.Engine as SDL.Eng
import qualified Helm.Engine.SDL.Graphics2D as SDL.G2D
import qualified Helm.Engine.SDL.Keyboard as SDL.Key
import qualified Helm.Engine.SDL.Mouse as SDL.Mouse
import qualified Helm.Graphics as H.Gs
import qualified Helm.Graphics2D as G2D
import qualified Helm.Graphics2D.Text as G2D.Text
import qualified Helm.Graphics2D.Transform as G2D.Tran
import qualified Helm.Keyboard as H.Key
import qualified Helm.Mouse as H.Mouse
import qualified Helm.Sub as H.Sub
import qualified Helm.Time as H.Time
import qualified Helm.Window as H.Win
----------
import Linear
import Control.Lens
import Data.Function (on)
import Data.Ord (comparing)

data Model -- data model
  = Model
  { player :: V2 Double,
    vel :: V2 Double
  } deriving Show

data Action -- sum of actions
  = Move (V2 Double) (V2 Double)
  | None

world :: [(V2 Double, V2 Double)]
world =
  [
    (V2 100 200, V2 120 220)
  ]

init' :: (Model, Cmd SDL.SDLEngine Action)
init' =
  ( Model
      { player = pure 0,
        vel = pure 0
      },
    H.Cmd.none
  )

upd :: Model -> Action -> (Model, Cmd SDL.SDLEngine Action)
upd m a = case a of
  Move v b -> (tick m {vel = max' b (vel m + v)}, H.Cmd.none)
    where
      max' (V2 a' b') (V2 c d) = V2 (maxa a' c) (maxa b' d)
      maxa x y = if abs x >= abs y then x else y
  None -> (tick m, H.Cmd.none)

subs :: H.Sub.Sub SDL.SDLEngine Action
subs =
  H.Sub.batch
    [ H.Key.downs downs,
      H.Time.every 1 every
    ]
  where
    downs H.Key.UpKey = Move (V2 0 (-5)) (V2 0 (-20))
    downs H.Key.RightKey = Move (V2 1 0) (V2 10 0)
    downs H.Key.LeftKey = Move (V2 (-1) 0) (V2 (-10) 0)
    downs _ = None
    every _ = None

view' :: Model -> Graphics SDL.SDLEngine
view' m =
  H.Gs.Graphics2D $ G2D.center (V2 800 600 / 2) $ G2D.collage
    [ G2D.filled (H.Color.rgb 0.4 0.4 0.4) $ G2D.rect $ V2 800 600,
      G2D.move (player m) $ G2D.filled (H.Color.rgb 1 0 0) $ G2D.circle 10,
      G2D.text $ G2D.Text.color (H.Color.rgb 1 1 1) $ G2D.Text.showText m,
      G2D.group (map (\(a, b) -> G2D.move a $ G2D.filled (H.Color.rgb 1 0 0) $ G2D.rect (abs (a - b))) world)
    ]
--------------------------------------------------
tick :: Model -> Model
tick m = case m of
  Model a b
    | a ^. _y > 300 -> Model (a & _y .~ 300) 0
    | otherwise -> Model (a + b) (b & _y %~ (+ 1))
