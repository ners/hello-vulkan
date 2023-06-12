module Vulkan.Shader
    ( createShaderModule
    , destroyShaderModule
    , withShaderModule
    , createFromFile
    , withShaderFile
    , module Vulkan.Core10.Shader
    )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.ByteString qualified as BS
import "vulkan" Vulkan qualified as Vk
import "vulkan" Vulkan.Core10.Shader hiding (createShaderModule, destroyShaderModule, withShaderModule)
import "vulkan" Vulkan.Zero qualified as Vk
import Prelude

createShaderModule :: (MonadIO m) => Vk.Device -> BS.ByteString -> m Vk.ShaderModule
createShaderModule device bytes =
    Vk.createShaderModule device (Vk.zero{Vk.code = bytes}) Nothing

destroyShaderModule :: (MonadIO m) => Vk.Device -> Vk.ShaderModule -> m ()
destroyShaderModule device shader =
    Vk.destroyShaderModule device shader Nothing

withShaderModule :: (MonadUnliftIO m) => Vk.Device -> BS.ByteString -> (Vk.ShaderModule -> m a) -> m a
withShaderModule device bytes f =
    withRunInIO $ \run ->
        bracket
            (createShaderModule device bytes)
            (destroyShaderModule device)
            (run . f)

createFromFile :: (MonadIO m) => FilePath -> Vk.Device -> m Vk.ShaderModule
createFromFile filePath device =
    liftIO $
        BS.readFile filePath >>= createShaderModule device

withShaderFile :: (MonadUnliftIO m) => FilePath -> Vk.Device -> (Vk.ShaderModule -> m a) -> m a
withShaderFile filePath device f = do
    bytes <- liftIO $ BS.readFile filePath
    withShaderModule device bytes f
