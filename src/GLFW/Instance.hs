module GLFW.Instance where

import Control.Exception (bracket_)
import Control.Monad (unless)
import Graphics.UI.GLFW qualified as GLFW
import Prelude

initialize :: IO ()
initialize =
    GLFW.init
        >>= flip unless (fail "Could not initialize GLFW")

withGLFW :: IO a -> IO a
withGLFW = bracket_ initialize GLFW.terminate
