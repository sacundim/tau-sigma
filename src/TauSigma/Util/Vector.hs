-- | Utilities for working with 'Vector's.  Mostly generation from pipes.
module TauSigma.Util.Vector
       ( readVector
       , readMVector
       , readMStream
       ) where
       
import Control.Monad.Primitive (PrimMonad, PrimState)

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
import Data.Vector.Fusion.Stream.Size (Size(..))

import Pipes (Producer, next)


-- | Construct a vector from a 'Producer'.  The producer will be
-- drained.
readVector :: (PrimMonad m, G.Vector v a) => Producer a m () -> m (v a)
readVector producer = readMVector producer >>= G.freeze

-- | Construct a mutable vector from a 'Producer'.  The producer will be
-- drained.
readMVector :: (PrimMonad m, M.MVector v a) =>
               Producer a m () -> m (v (PrimState m) a)
readMVector producer = M.munstream (readMStream producer)
  
-- | Construct a vector stream from a producer.  The producer will be
-- drained.
readMStream :: Monad m => Producer a m () -> Stream m a
readMStream producer = Stream go producer Unknown
  where
    go s = do
      state <- next s
      return (step state)
    step :: Either r (a, Producer a m r) -> Step (Producer a m r) a
    step (Left _) = Done
    step (Right (a, k)) = Yield a k
