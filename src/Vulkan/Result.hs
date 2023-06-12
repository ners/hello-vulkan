module Vulkan.Result where

import Control.Monad.IO.Class (MonadIO)
import Vulkan.Exception (throwVk)
import "vulkan" Vulkan qualified as Vk
import Prelude

expect :: (MonadIO m) => [Vk.Result] -> m (Vk.Result, a) -> m a
expect expectation action = do
    (result, a) <- action
    if result `elem` expectation
        then pure a
        else throwVk result

expect_ :: (MonadIO m) => [Vk.Result] -> m Vk.Result -> m ()
expect_ expectation action = do
    result <- action
    if result `elem` expectation
        then pure ()
        else throwVk result
