module Vulkan.VirtualFrame where

-- base
import Prelude
import Control.Monad.IO.Class (MonadIO)

-- vector
import Data.Vector qualified as V

-- vulkan
import "vulkan" Vulkan qualified as Vk

import Vulkan.Exception (throwVk)
import Vulkan.Fence     (waitForFence)

newtype FrameIndex = FrameIndex { value :: Int }
  deriving stock (Enum, Eq, Integral, Num, Ord, Real, Show)

data VirtualFrame = VirtualFrame
  { commandBuffer    :: Vk.CommandBuffer
  , acquireSemaphore :: Vk.Semaphore
  , renderSemaphore  :: Vk.Semaphore
  , renderFence      :: Vk.Fence
  }
  deriving stock (Eq, Show)

getNextVirtualFrame
  :: MonadIO m
  => Vk.Device
  -> V.Vector VirtualFrame
  -> FrameIndex
  -> m (VirtualFrame, FrameIndex)
getNextVirtualFrame device frames = getNext
  where
    getNext ix = do
      let
        frame = frames V.! ix.value
        next  = (ix + 1) `mod` FrameIndex (V.length frames)
      fenceResult <-
        waitForFence
          device
          frame.renderFence
          1_000_000_000
      case fenceResult of
        Vk.SUCCESS  -> pure (frame, next)
        Vk.TIMEOUT  -> getNext next
        errorResult -> throwVk errorResult
{-# INLINE getNextVirtualFrame #-}