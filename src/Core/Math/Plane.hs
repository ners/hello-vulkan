module Core.Math.Plane where

import Core.Math.Ray (Ray (..))
import Core.Math.Vec3 (Vec3, dot, scalarMultiply)
import Prelude

data Plane = Plane
    { point :: !Vec3
    , normal :: !Vec3
    }

intersectRay :: Ray -> Plane -> Vec3
intersectRay (Ray origin direction) (Plane point normal) =
    origin + t `scalarMultiply` direction
  where
    t =
        dot normal (point - origin)
            / dot normal direction
