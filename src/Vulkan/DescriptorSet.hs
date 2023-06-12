module Vulkan.DescriptorSet
    ( createDescriptorSetLayout
    , destroyDescriptorSetLayout
    , withDescriptorSetLayout
    , createDescriptorPool
    , destroyDescriptorPool
    , withDescriptorPool
    , resetDescriptorPool
    , withDescriptorSets
    , module Vulkan.Core10.DescriptorSet
    )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.Vector qualified as V
import "vulkan" Vulkan qualified as Vk
import "vulkan" Vulkan.CStruct.Extends qualified as Vk
import "vulkan" Vulkan.Core10.DescriptorSet hiding
    ( createDescriptorPool
    , createDescriptorSetLayout
    , destroyDescriptorPool
    , destroyDescriptorSetLayout
    , resetDescriptorPool
    , withDescriptorPool
    , withDescriptorSetLayout
    , withDescriptorSets
    )
import "vulkan" Vulkan.Zero qualified as Vk
import Prelude

createDescriptorSetLayout
    :: (Vk.Extendss Vk.DescriptorSetLayoutCreateInfo a, Vk.PokeChain a, MonadIO m)
    => Vk.Device
    -> Vk.DescriptorSetLayoutCreateInfo a
    -> m Vk.DescriptorSetLayout
createDescriptorSetLayout device info =
    Vk.createDescriptorSetLayout device info Nothing

destroyDescriptorSetLayout :: (MonadIO m) => Vk.Device -> Vk.DescriptorSetLayout -> m ()
destroyDescriptorSetLayout device layout =
    Vk.destroyDescriptorSetLayout device layout Nothing

withDescriptorSetLayout
    :: (Vk.Extendss Vk.DescriptorSetLayoutCreateInfo a, Vk.PokeChain a, MonadUnliftIO m)
    => Vk.Device
    -> Vk.DescriptorSetLayoutCreateInfo a
    -> (Vk.DescriptorSetLayout -> m b)
    -> m b
withDescriptorSetLayout device info f =
    withRunInIO $ \run ->
        bracket
            (createDescriptorSetLayout device info)
            (destroyDescriptorSetLayout device)
            (run . f)

createDescriptorPool
    :: (Vk.Extendss Vk.DescriptorPoolCreateInfo a, Vk.PokeChain a, MonadIO m)
    => Vk.Device
    -> Vk.DescriptorPoolCreateInfo a
    -> m Vk.DescriptorPool
createDescriptorPool device info =
    Vk.createDescriptorPool device info Nothing
{-# INLINE createDescriptorPool #-}

destroyDescriptorPool :: (MonadIO m) => Vk.Device -> Vk.DescriptorPool -> m ()
destroyDescriptorPool device pool =
    Vk.destroyDescriptorPool device pool Nothing
{-# INLINE destroyDescriptorPool #-}

withDescriptorPool
    :: (Vk.Extendss Vk.DescriptorPoolCreateInfo a, Vk.PokeChain a, MonadUnliftIO m)
    => Vk.Device
    -> Vk.DescriptorPoolCreateInfo a
    -> (Vk.DescriptorPool -> m b)
    -> m b
withDescriptorPool device info f =
    withRunInIO $ \run ->
        bracket
            (createDescriptorPool device info)
            (destroyDescriptorPool device)
            (run . f)

resetDescriptorPool :: (MonadIO m) => Vk.Device -> Vk.DescriptorPool -> m ()
resetDescriptorPool device pool =
    Vk.resetDescriptorPool device pool Vk.zero

withDescriptorSets
    :: (Vk.Extendss Vk.DescriptorSetAllocateInfo a, Vk.PokeChain a, MonadUnliftIO m)
    => Vk.Device
    -> Vk.DescriptorSetAllocateInfo a
    -> (V.Vector Vk.DescriptorSet -> m b)
    -> m b
withDescriptorSets device info f =
    withRunInIO $ \run ->
        bracket
            (allocateDescriptorSets device info)
            (freeDescriptorSets device (Vk.descriptorPool info))
            (run . f)
