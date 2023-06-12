module Main where

import Prelude
import Control.Concurrent (threadDelay)
import GLFW.Instance
import GLFW.Window
import Vulkan.Window

main :: IO ()
main = withGLFW $ \glfw -> do
    let strategy = glfwStrategy glfw
    withWindow strategy 800 600 "Hello Vulkan" $ \window -> do
        threadDelay 5_000_000
