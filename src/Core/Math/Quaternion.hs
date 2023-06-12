module Core.Math.Quaternion where

import Prelude

data Quaternion
    = Quaternion
        {-# UNPACK #-} !Float
        {-# UNPACK #-} !Float
        {-# UNPACK #-} !Float
        {-# UNPACK #-} !Float
    deriving stock (Eq, Ord, Read, Show)
