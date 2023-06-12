module Vulkan.Command
    ( pushStorableConstants
    , module Vulkan.Core10.CommandBufferBuilding
    )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, sizeOf)
import "vulkan" Vulkan qualified as Vk
import "vulkan" Vulkan.Core10.CommandBufferBuilding
import Prelude

pushStorableConstants
    :: (Storable a, MonadIO m)
    => Vk.CommandBuffer
    -> Vk.PipelineLayout
    -> Vk.ShaderStageFlags
    -> Word32
    -> [a]
    -> m ()
pushStorableConstants buffer layout flags offset values =
    liftIO $
        withArrayLen values $ \n ptr ->
            Vk.cmdPushConstants
                (buffer)
                (layout)
                (flags)
                (offset)
                (fromIntegral $ n * sizeOf (values !! 0))
                (castPtr ptr)
{-# INLINE pushStorableConstants #-}
