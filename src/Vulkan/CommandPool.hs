module Vulkan.CommandPool
    ( createCommandPool
    , destroyCommandPool
    , withCommandPool
    , module Vulkan.Core10.CommandPool
    )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import "vulkan" Vulkan qualified as Vk
import "vulkan" Vulkan.Core10.CommandPool hiding (createCommandPool, destroyCommandPool, withCommandPool)
import Prelude

createCommandPool :: (MonadIO m) => Vk.Device -> Vk.CommandPoolCreateInfo -> m Vk.CommandPool
createCommandPool device info =
    Vk.createCommandPool device info Nothing
{-# INLINE createCommandPool #-}

destroyCommandPool :: (MonadIO m) => Vk.Device -> Vk.CommandPool -> m ()
destroyCommandPool device pool =
    Vk.destroyCommandPool device pool Nothing
{-# INLINE destroyCommandPool #-}

withCommandPool :: (MonadUnliftIO m) => Vk.Device -> Vk.CommandPoolCreateInfo -> (Vk.CommandPool -> m a) -> m a
withCommandPool device info f =
    withRunInIO $ \run ->
        bracket
            (createCommandPool device info)
            (destroyCommandPool device)
            (run . f)
