module Vulkan.CommandBuffer
    ( withCommandBuffers
    , module Vulkan.Core10.CommandBuffer
    )
where

import Control.Exception (bracket)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.Vector qualified as V
import "vulkan" Vulkan qualified as Vk
import "vulkan" Vulkan.Core10.CommandBuffer hiding (withCommandBuffers)
import Prelude

withCommandBuffers
    :: (MonadUnliftIO m)
    => Vk.Device
    -> Vk.CommandBufferAllocateInfo
    -> (V.Vector Vk.CommandBuffer -> m a)
    -> m a
withCommandBuffers device info f =
    withRunInIO $ \run ->
        bracket
            (allocateCommandBuffers device info)
            (freeCommandBuffers device (Vk.commandPool info))
            (run . f)
