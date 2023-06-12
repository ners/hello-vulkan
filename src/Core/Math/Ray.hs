module Core.Math.Ray where

import Core.Math.Vec3 (Vec3)
import Prelude

data Ray = Ray
    { origin :: !Vec3
    , direction :: !Vec3
    }
    deriving stock (Eq, Ord, Read, Show)
