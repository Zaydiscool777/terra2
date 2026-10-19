module Main (main) where

import Helm
import Helm.Engine
import Helm.Engine.SDL
import Lib

main :: IO ()
main = do
  sdl <- startup
  run sdl (GameConfig init' upd subs view')
  putStrLn "end"
