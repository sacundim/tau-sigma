-- | Friendlier wrappers around "Pipes.Vector".
module TauSigma.Vector
       ( readVector
       ) where

import Control.Monad.Primitive (PrimMonad)

import Data.Vector.Generic (Vector)

import Pipes
import Pipes.Vector

readVector :: (PrimMonad m, Vector v a) => Producer a m () -> m (v a)
readVector prod = runEffect $ runToVectorP (hoist lift prod >-> toVector)

