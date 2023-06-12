{-# LANGUAGE DataKinds #-}

module Vulkan.Instance
    ( InstanceBuilder
    , setAppName
    , setAppVersion
    , setEngineName
    , setEngineVersion
    , setApiVersion
    , enableLayer
    , enableLayers
    , enableVulkanExtensions
    , createInstance
    , destroyInstance
    , withInstance
    )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Word (Word32)
import Vulkan.Builder (Builder, execBuilder, modify)
import Vulkan.Version (version)
import "vulkan" Vulkan.Core10.DeviceInitialization qualified as Vk
import "vulkan" Vulkan.Zero qualified as Vk
import Prelude

newtype InstanceBuilder a = InstanceBuilder {builder :: Builder (Vk.InstanceCreateInfo '[]) a}
    deriving stock (Functor)
    deriving newtype (Applicative, Monad, MonadFail, MonadIO)

updateAppInfo :: (Vk.ApplicationInfo -> Vk.ApplicationInfo) -> InstanceBuilder ()
updateAppInfo f =
    InstanceBuilder $
        modify $ \createInfo ->
            let
                appInfo = fromMaybe Vk.zero (Vk.applicationInfo createInfo)
             in
                createInfo{Vk.applicationInfo = Just $ f appInfo}

setAppName :: String -> InstanceBuilder ()
setAppName name =
    updateAppInfo $
        \info -> info{Vk.applicationName = Just $ BS.pack name}

setAppVersion :: Word32 -> Word32 -> Word32 -> InstanceBuilder ()
setAppVersion major minor patch =
    updateAppInfo $
        \info -> info{Vk.applicationVersion = version major minor patch}

setEngineName :: String -> InstanceBuilder ()
setEngineName name =
    updateAppInfo $
        \info -> info{Vk.engineName = Just $ BS.pack name}

setEngineVersion :: Word32 -> Word32 -> Word32 -> InstanceBuilder ()
setEngineVersion major minor patch =
    updateAppInfo $
        \info -> info{Vk.engineVersion = version major minor patch}

setApiVersion :: Word32 -> Word32 -> Word32 -> InstanceBuilder ()
setApiVersion major minor patch =
    updateAppInfo $
        -- little bit different from the others to disambiguate "apiVersion"
        \(Vk.ApplicationInfo n av en ev _) ->
            Vk.ApplicationInfo n av en ev (version major minor patch)

enableLayer :: String -> InstanceBuilder ()
enableLayer name = enableLayers [name]

enableLayers :: [String] -> InstanceBuilder ()
enableLayers names =
    InstanceBuilder $
        modify $ \info ->
            info
                { Vk.enabledLayerNames =
                    (V.++)
                        (Vk.enabledLayerNames info)
                        (V.fromList $ fmap BS.pack names)
                }

enableVulkanExtensions :: [String] -> InstanceBuilder ()
enableVulkanExtensions names =
    InstanceBuilder $
        modify $ \info ->
            info
                { Vk.enabledExtensionNames =
                    (V.++)
                        (Vk.enabledExtensionNames info)
                        (V.fromList $ fmap BS.pack names)
                }

createInstance :: (MonadIO m) => InstanceBuilder () -> m Vk.Instance
createInstance instanceBuilder =
    liftIO $ do
        info <- execBuilder Vk.zero instanceBuilder.builder
        Vk.createInstance info Nothing

destroyInstance :: (MonadIO m) => Vk.Instance -> m ()
destroyInstance = flip Vk.destroyInstance Nothing

withInstance :: (MonadUnliftIO m) => InstanceBuilder () -> (Vk.Instance -> m a) -> m a
withInstance builder f =
    withRunInIO $ \run ->
        bracket
            (createInstance builder)
            (destroyInstance)
            (run . f)
