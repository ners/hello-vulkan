{-# LANGUAGE OverloadedLists #-}
module Vulkan.Queue
  ( QueueFamily
  , QueueIndex
  , Queue(..)
  , hasFlags
  , hasCompute
  , hasGraphics
  , hasTransfer
  , submitOne
  , module Vulkan.Core10.Queue
  ) where

-- base
import Prelude
import Control.Monad.IO.Class (MonadIO)
import Data.Bits              ((.&.))
import Data.Word              (Word32)

-- vulkan
import "vulkan" Vulkan                 qualified as Vk
import "vulkan" Vulkan.Core10.Queue    hiding(Queue)
import "vulkan" Vulkan.CStruct.Extends qualified as Vk

type QueueFamily = Word32

type QueueIndex = Word32

data Queue = Queue
  { handle      :: Vk.Queue
  , familyIndex :: QueueFamily
  , queueIndex  :: QueueIndex
  }
  deriving stock (Eq, Show)

hasFlags :: Vk.QueueFlags -> Vk.QueueFamilyProperties -> Bool
hasFlags flags props =
     Vk.queueFlags props .&. flags == flags
  && Vk.queueCount props > 0

hasCompute :: Vk.QueueFamilyProperties -> Bool
hasCompute = hasFlags Vk.QUEUE_COMPUTE_BIT

hasGraphics :: Vk.QueueFamilyProperties -> Bool
hasGraphics = hasFlags Vk.QUEUE_GRAPHICS_BIT

hasTransfer :: Vk.QueueFamilyProperties -> Bool
hasTransfer = hasFlags Vk.QUEUE_TRANSFER_BIT

submitOne
  :: (Vk.Extendss Vk.SubmitInfo a, Vk.PokeChain a, Show (Vk.Chain a), MonadIO m)
  => Queue -> Vk.Fence -> Vk.SubmitInfo a -> m ()
submitOne queue fence info =
  Vk.queueSubmit queue.handle [Vk.SomeStruct info] fence
{-# INLINE submitOne #-}
