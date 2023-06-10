{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module GLFW.Window where
import Graphics.UI.GLFW qualified as GLFW
import Data.Maybe (fromJust)
import Foreign.C (peekCString)
import Data.Vector qualified as V
import Data.ByteString.Char8 qualified as BS

import Vulkan qualified as Vk'
import Vulkan.Core10.DeviceInitialization qualified as Vk
import Vulkan.Zero qualified as Vk
import Data.Word (Word32, Word64)
import Data.Function ((&))
import Data.Bits (shiftL, (.|.))
import Foreign (peek, nullPtr, Ptr)
import Foreign.Marshal (alloca)

data GlfwWindow = GlfwWindow {
  handle :: GLFW.Window,
  inst :: Vk.Instance,
  surface :: Vk'.SurfaceKHR
}

version :: Word32 -> Word32 -> Word32 -> Word32
version major minor patch =
  shiftL major 22 .|. shiftL minor 12 .|. patch

setVkApiVersion :: Word32 -> Word32 -> Word32 -> Vk.ApplicationInfo -> Vk.ApplicationInfo
setVkApiVersion major minor patch (Vk.ApplicationInfo n av en ev _) = Vk.ApplicationInfo n av en ev (version major minor patch)

createWindow :: Int -> Int -> IO GlfwWindow
createWindow width height = do
  GLFW.defaultWindowHints
  GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
  handle <- fromJust <$> GLFW.createWindow width height "Hello, Vulkan!" Nothing Nothing
  cexts <- GLFW.getRequiredInstanceExtensions
  exts <- mapM peekCString cexts
  print exts
  let info :: Vk.InstanceCreateInfo '[] = Vk.zero {
    Vk.applicationInfo = Just $ (Vk.zero {
      Vk.applicationName = Just "MWE",
      Vk.applicationVersion = version 0 0 1,
      Vk.engineName = Just "MWE",
      Vk.engineVersion = version 0 0 1
    }) & setVkApiVersion 1 3 0,
    Vk.enabledExtensionNames = V.fromList $ BS.pack <$> exts,
    Vk.enabledLayerNames = V.fromList $ BS.pack <$> [ "VK_LAYER_KHRONOS_validation" ]
  }
  inst <- Vk.createInstance info Nothing
  surface <- Vk'.SurfaceKHR <$> alloca (\(ptr :: Ptr Word64) -> do
    result <- GLFW.createWindowSurface (Vk.instanceHandle inst) handle nullPtr ptr
    if result == 0 then
      peek ptr
    else
      fail "horribly")
  pure GlfwWindow {..}

destroyWindow :: GlfwWindow -> IO ()
destroyWindow w = do
  Vk'.destroySurfaceKHR w.inst w.surface Nothing
  Vk.destroyInstance w.inst Nothing
  GLFW.destroyWindow w.handle
