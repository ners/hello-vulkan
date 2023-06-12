module Core.Math.Ray where

import Prelude
import Core.Math.Vec3 (Vec3)

data Ray = Ray
    { origin :: !Vec3
    , direction :: !Vec3
    }
    deriving stock (Eq, Ord, Read, Show)
