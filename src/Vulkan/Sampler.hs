module Vulkan.Sampler
    ( createSampler
    , destroySampler
    , withSampler
    , module Vulkan.Core10.Sampler
    )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import "vulkan" Vulkan qualified as Vk
import "vulkan" Vulkan.CStruct.Extends qualified as Vk
import "vulkan" Vulkan.Core10.Sampler hiding (createSampler, destroySampler, withSampler)
import Prelude

createSampler
    :: (Vk.Extendss Vk.SamplerCreateInfo a, Vk.PokeChain a, MonadIO m)
    => Vk.Device
    -> Vk.SamplerCreateInfo a
    -> m Vk.Sampler
createSampler device info =
    Vk.createSampler device info Nothing

destroySampler :: (MonadIO m) => Vk.Device -> Vk.Sampler -> m ()
destroySampler device sampler =
    Vk.destroySampler device sampler Nothing

withSampler
    :: (Vk.Extendss Vk.SamplerCreateInfo a, Vk.PokeChain a, MonadUnliftIO m)
    => Vk.Device
    -> Vk.SamplerCreateInfo a
    -> (Vk.Sampler -> m b)
    -> m b
withSampler device info f =
    withRunInIO $ \run ->
        bracket
            (createSampler device info)
            (destroySampler device)
            (run . f)
