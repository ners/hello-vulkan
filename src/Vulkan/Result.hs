module Vulkan.Result where

-- base
import Prelude
import Control.Monad.IO.Class (MonadIO)

-- vulkan
import "vulkan" Vulkan qualified as Vk

import Vulkan.Exception (throwVk)

expect :: MonadIO m => [Vk.Result] -> m (Vk.Result, a) -> m a
expect expectation action = do
  (result, a) <- action
  if result `elem` expectation then
    pure a
  else
    throwVk result

expect_ :: MonadIO m => [Vk.Result] -> m Vk.Result -> m ()
expect_ expectation action = do
  result <- action
  if result `elem` expectation then
    pure ()
  else
    throwVk result
