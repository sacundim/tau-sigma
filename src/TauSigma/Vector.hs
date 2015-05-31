-- | Friendlier wrappers around "Pipes.Vector".
module TauSigma.Vector
       ( readVector
       , consumeVector
       ) where
       
import Control.Monad.Primitive (PrimMonad)

import Data.Vector.Generic (Vector)

import Pipes
import Pipes.Vector


readVector :: (PrimMonad m, Vector v a) => Producer a m () -> m (v a)
readVector prod = fromProducer (hoist lift prod)

consumeVector :: (PrimMonad m, Vector v a) => Consumer a m (v a)
consumeVector = runToVectorP toVector

