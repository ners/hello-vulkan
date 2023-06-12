module Vulkan.Semaphore
  ( createSemaphore
  , destroySemaphore
  , withSemaphore
  , module Vulkan.Core10.QueueSemaphore
  ) where

-- base
import Prelude
import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO)

-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- vulkan
import "vulkan" Vulkan                       qualified as Vk
import "vulkan" Vulkan.Core10.QueueSemaphore hiding (createSemaphore, destroySemaphore, withSemaphore)
import "vulkan" Vulkan.Zero                  qualified as Vk

createSemaphore :: MonadIO m => Vk.Device -> m Vk.Semaphore
createSemaphore device =
  Vk.createSemaphore device Vk.zero Nothing

destroySemaphore :: MonadIO m => Vk.Device -> Vk.Semaphore -> m ()
destroySemaphore device semaphore =
  Vk.destroySemaphore device semaphore Nothing

withSemaphore :: MonadUnliftIO m => Vk.Device -> (Vk.Semaphore -> m a) -> m a
withSemaphore device f =
  withRunInIO $ \run ->
    bracket
      ( createSemaphore device )
      ( destroySemaphore device )
      ( run . f )
