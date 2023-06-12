module Vulkan.PipelineLayout
    ( createPipelineLayout
    , destroyPipelineLayout
    , withPipelineLayout
    , module Vulkan.Core10.PipelineLayout
    )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import "vulkan" Vulkan qualified as Vk
import "vulkan" Vulkan.Core10.PipelineLayout hiding (createPipelineLayout, destroyPipelineLayout, withPipelineLayout)
import Prelude

createPipelineLayout :: (MonadIO m) => Vk.Device -> Vk.PipelineLayoutCreateInfo -> m Vk.PipelineLayout
createPipelineLayout device info =
    Vk.createPipelineLayout device info Nothing

destroyPipelineLayout :: (MonadIO m) => Vk.Device -> Vk.PipelineLayout -> m ()
destroyPipelineLayout device layout =
    Vk.destroyPipelineLayout device layout Nothing

withPipelineLayout
    :: (MonadUnliftIO m)
    => Vk.Device
    -> Vk.PipelineLayoutCreateInfo
    -> (Vk.PipelineLayout -> m a)
    -> m a
withPipelineLayout device info f =
    withRunInIO $ \run ->
        bracket
            (createPipelineLayout device info)
            (destroyPipelineLayout device)
            (run . f)
