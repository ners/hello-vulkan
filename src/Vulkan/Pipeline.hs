module Vulkan.Pipeline
    ( createGraphicsPipelines
    , destroyPipeline
    , withGraphicsPipelines
    , module Vulkan.Core10.Pipeline
    )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.Vector qualified as V
import Vulkan.Result (expect)
import "vulkan" Vulkan qualified as Vk
import "vulkan" Vulkan.CStruct.Extends qualified as Vk
import "vulkan" Vulkan.Core10.Pipeline hiding (createGraphicsPipelines, destroyPipeline, withGraphicsPipelines)
import Prelude

createGraphicsPipelines
    :: (MonadIO m)
    => Vk.Device
    -> Vk.PipelineCache
    -> V.Vector (Vk.SomeStruct Vk.GraphicsPipelineCreateInfo)
    -> m (V.Vector Vk.Pipeline)
createGraphicsPipelines device cache infos =
    expect [Vk.SUCCESS] $
        Vk.createGraphicsPipelines device cache infos Nothing

destroyPipeline :: (MonadIO m) => Vk.Device -> Vk.Pipeline -> m ()
destroyPipeline device pipeline =
    Vk.destroyPipeline device pipeline Nothing

withGraphicsPipelines
    :: (MonadUnliftIO m)
    => Vk.Device
    -> Vk.PipelineCache
    -> V.Vector (Vk.SomeStruct Vk.GraphicsPipelineCreateInfo)
    -> (V.Vector Vk.Pipeline -> m a)
    -> m a
withGraphicsPipelines device cache infos f =
    withRunInIO $ \run ->
        bracket
            (createGraphicsPipelines device cache infos)
            (flip V.forM_ $ destroyPipeline device)
            (run . f)
