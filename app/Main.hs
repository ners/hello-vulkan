module Main where

import Prelude
import Control.Concurrent (threadDelay)
import GLFW.Instance (withGLFW)
import GLFW.Window qualified as GLFW

main :: IO ()
main = withGLFW $ do
    w <- GLFW.createWindow 800 600
    putStrLn "create window"
    threadDelay 5_000_000
    putStrLn "destroy window"
    GLFW.destroyWindow w
