module Vulkan.Frame where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Data.Vector qualified as V
import Vulkan.Swapchain qualified as S
import Vulkan.VirtualFrame qualified as F
import "vulkan" Vulkan qualified as Vk
import Prelude

data Frame = Frame
    { commandBuffer :: Vk.CommandBuffer
    , acquireSemaphore :: Vk.Semaphore
    , renderSemaphore :: Vk.Semaphore
    , renderFence :: Vk.Fence
    , image :: Vk.Image
    , imageIndex :: Int
    , virtualIndex :: Int
    }

getNextFrame
    :: (MonadIO m)
    => S.Swapchain
    -> V.Vector F.VirtualFrame
    -> F.FrameIndex
    -> m (Maybe Frame, F.FrameIndex)
getNextFrame swapchain frames ix = do
    (virtualFrame, newIx) <-
        F.getNextVirtualFrame swapchain.device frames ix
    imageResult <-
        S.getNextImage
            1_000_000_000
            virtualFrame.acquireSemaphore
            Vk.NULL_HANDLE
            swapchain
    frame <-
        forM (S.indexOf imageResult) $ \imgIx ->
            let
                intIx = fromIntegral imgIx
             in
                pure $
                    Frame
                        { commandBuffer = virtualFrame.commandBuffer
                        , acquireSemaphore = virtualFrame.acquireSemaphore
                        , renderSemaphore = virtualFrame.renderSemaphore
                        , renderFence = virtualFrame.renderFence
                        , image = swapchain.images V.! intIx
                        , imageIndex = intIx
                        , virtualIndex = newIx.value
                        }
    pure (frame, newIx)
