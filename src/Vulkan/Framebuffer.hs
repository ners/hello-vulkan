module Vulkan.Framebuffer
    ( createFramebuffer
    , destroyFramebuffer
    , withFramebuffer
    )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import "vulkan" Vulkan qualified as Vk
import "vulkan" Vulkan.CStruct.Extends qualified as Vk
import Prelude

createFramebuffer
    :: (Vk.Extendss Vk.FramebufferCreateInfo a, Vk.PokeChain a, MonadIO m)
    => Vk.Device
    -> Vk.FramebufferCreateInfo a
    -> m Vk.Framebuffer
createFramebuffer device info =
    Vk.createFramebuffer device info Nothing

destroyFramebuffer :: (MonadIO m) => Vk.Device -> Vk.Framebuffer -> m ()
destroyFramebuffer device buffer =
    Vk.destroyFramebuffer device buffer Nothing

withFramebuffer
    :: (Vk.Extendss Vk.FramebufferCreateInfo a, Vk.PokeChain a, MonadUnliftIO m)
    => Vk.Device
    -> Vk.FramebufferCreateInfo a
    -> (Vk.Framebuffer -> m b)
    -> m b
withFramebuffer device info f =
    withRunInIO $ \run ->
        bracket
            (createFramebuffer device info)
            (destroyFramebuffer device)
            (run . f)
