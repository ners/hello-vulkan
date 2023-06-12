module Vulkan.Exception
    ( VulkanException (..)
    , throwIO
    , throwVk
    )
where

import Control.Exception qualified as E
import Control.Monad.IO.Class (MonadIO, liftIO)
import "vulkan" Vulkan qualified as Vk
import "vulkan" Vulkan.Exception qualified as Vk
import Prelude

data VulkanException
    = InternalError
    | NoGraphicsQueueFamily
    | NoPresentationQueueFamily
    | NoSuitableDevice
    | NoSuitablePhysicalDevice
    | NoSuitableQueue
    | NoSurfaceFormat
    deriving stock (Eq, Ord, Show)

instance E.Exception VulkanException

throwIO :: (E.Exception e, MonadIO m) => e -> m a
throwIO = liftIO . E.throwIO

throwVk :: (MonadIO m) => Vk.Result -> m a
throwVk = throwIO . Vk.VulkanException
