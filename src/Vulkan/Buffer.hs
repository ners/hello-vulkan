module Vulkan.Buffer
    ( createBuffer
    , destroyBuffer
    , withBuffer
    , module Vulkan.Core10.Buffer
    )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import "vulkan" Vulkan qualified as Vk
import "vulkan" Vulkan.CStruct.Extends qualified as Vk
import "vulkan" Vulkan.Core10.Buffer hiding (createBuffer, destroyBuffer, withBuffer)
import Prelude

createBuffer
    :: (Vk.Extendss Vk.BufferCreateInfo a, Vk.PokeChain a, MonadIO m)
    => Vk.Device
    -> Vk.BufferCreateInfo a
    -> m Vk.Buffer
createBuffer device info =
    Vk.createBuffer device info Nothing

destroyBuffer :: (MonadIO m) => Vk.Device -> Vk.Buffer -> m ()
destroyBuffer device buffer =
    Vk.destroyBuffer device buffer Nothing

withBuffer
    :: (Vk.Extendss Vk.BufferCreateInfo a, Vk.PokeChain a, MonadUnliftIO m)
    => Vk.Device
    -> Vk.BufferCreateInfo a
    -> (Vk.Buffer -> m b)
    -> m b
withBuffer device info f =
    withRunInIO $ \run ->
        bracket
            (createBuffer device info)
            (destroyBuffer device)
            (run . f)
