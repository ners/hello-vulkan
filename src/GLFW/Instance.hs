module GLFW.Instance where

import Graphics.UI.GLFW qualified as GLFW
import Control.Monad (unless)
import Control.Exception (bracket_)

initialize :: IO ()
initialize = GLFW.init >>=
  flip unless (fail "Could not initialize GLFW")

withGLFW :: IO a -> IO a
withGLFW = bracket_ initialize GLFW.terminate