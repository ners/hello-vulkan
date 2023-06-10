module Main where

import GLFW.Instance (withGLFW)
import GLFW.Window qualified as GLFW
import Control.Concurrent (threadDelay)

main :: IO ()
main = withGLFW $ do
  w <- GLFW.createWindow 800 600
  putStrLn "create window"
  threadDelay 5_000_000
  putStrLn "destroy window"
  GLFW.destroyWindow w
