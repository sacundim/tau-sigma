{-# LANGUAGE ScopedTypeVariables #-}

-- | Utilities for working with 'Vector's.  Mostly generation from pipes.
module TauSigma.Util.Vector
       ( drainToVector
       , drainToMVector
       , drainToMStream
       , takeVector
       , takeMVector
       , takeMStream
       ) where
       
import Control.Monad.Primitive (PrimMonad, PrimState)

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Fusion.Stream.Monadic (Stream(..), unfoldrM, sized)
import Data.Vector.Fusion.Stream.Size (Size(..))

import Pipes (Producer, next, (>->))
import qualified Pipes.Prelude as P


-- | Construct a vector from a 'Producer'.  The producer will be
-- drained.
drainToVector :: (PrimMonad m, G.Vector v a) => Producer a m () -> m (v a)
{-# INLINE drainToVector #-}
drainToVector producer = drainToMVector producer >>= G.freeze

-- | Construct a mutable vector from a 'Producer'.  The producer will be
-- drained.
drainToMVector :: (PrimMonad m, M.MVector v a) =>
               Producer a m () -> m (v (PrimState m) a)
{-# INLINE drainToMVector #-}
drainToMVector producer = M.munstream (drainToMStream producer)
  
-- | Drain the output of a 'Producer' and shove it into a monadic 'Stream'.
drainToMStream :: Monad m => Producer a m () -> Stream m a
{-# INLINE drainToMStream #-}
drainToMStream = unfoldrM gen
  where gen producer = do
          state <- next producer
          return $ case state of
           Left _ -> Nothing
           Right x -> Just x

{-
  Stream go producer Unknown
  where
    go s = do
      state <- next s
      return (step state)
    step :: Either r (a, Producer a m r) -> Step (Producer a m r) a
    step (Left _) = Done
    step (Right (a, k)) = Yield a k
-}

takeVector :: (PrimMonad m, G.Vector v a) => Int -> Producer a m () -> m (v a)
{-# INLINE takeVector #-}
takeVector n producer = takeMVector n producer >>= G.freeze

-- | Take up to @n@ items from the 'Producer' and shove them into a
-- mutable vector.
--
-- TODO: is there a better way of doing this?
--
takeMVector :: (PrimMonad m, M.MVector v a) =>
                 Int -> Producer a m () -> m (v (PrimState m) a)
{-# INLINE takeMVector #-}
takeMVector n producer = M.munstream (takeMStream n producer)

-- | Take up to @n@ items from the 'Producer' and shove them into a
-- monadic 'Stream'.
takeMStream :: Monad m => Int -> Producer a m () -> Stream m a
{-# INLINE takeMStream #-}
takeMStream n producer = drainToMStream (producer >-> P.take n) `sized` Max n
