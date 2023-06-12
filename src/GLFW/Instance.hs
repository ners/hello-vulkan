module GLFW.Instance (GLFW, initialize, terminate, withGLFW) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Graphics.UI.GLFW qualified as GLFW
import Prelude

data GLFW = GLFW

initialize :: IO GLFW
initialize =
    GLFW.init
        >>= flip unless (fail "Could not initialize GLFW.")
        >> pure GLFW

terminate :: IO ()
terminate = GLFW.terminate

withGLFW :: (GLFW -> IO a) -> IO a
withGLFW = bracket initialize (const terminate)
