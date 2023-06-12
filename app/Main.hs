module Main where

import Control.Concurrent (threadDelay)
import GLFW
import Vulkan
import Prelude

main :: IO ()
main = withGLFW $ \glfw -> do
    let strategy = glfwStrategy glfw
    withWindow strategy 800 600 "Hello Vulkan" $ \window -> do
        extensions <- getExtensions strategy window
        print extensions
        let instanceConfig = do
                setAppName "hello-vulkan"
                setAppVersion 0 0 0
                setApiVersion 1 3 0
                setEngineName "hello-vulkan"
                setEngineVersion 0 0 0
                enableVulkanExtensions extensions
                enableLayers [] --["VK_LAYER_KHRONOS_validation"]
        withInstance instanceConfig $ \inst -> do
            threadDelay 5_000_000
