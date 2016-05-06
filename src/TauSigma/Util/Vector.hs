{-# LANGUAGE ScopedTypeVariables #-}

-- | Utilities for working with 'Vector's.  Mostly generation from pipes.
module TauSigma.Util.Vector
       ( each
       , ieach
       , drainToVector
       , drainToMVector
       , drainToMStream
       , takeVector
       , takeMVector
       , takeMStream
       ) where
       
import Control.Monad.Primitive (PrimMonad, PrimState)

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Fusion.Bundle.Size (Size(..))
import Data.Vector.Fusion.Bundle.Monadic (Bundle, unfoldrM, sized)

import Pipes (Producer, yield, next, (>->))
import qualified Pipes.Prelude as P

-- | Like @each@ from "Pipes" but tied to the 'Vector' class
-- instead of @Foldable@.  So works on, e.g., unboxed vectors.
each :: (Monad m, G.Vector v a) => v a -> Producer a m ()
{-# INLINE each #-}
each = G.foldr go (return ())
  where go a as = yield a >> as

ieach :: (Monad m, G.Vector v a) => v a -> Producer (Int, a) m ()
{-# INLINE ieach #-}
ieach = G.ifoldr go (return ())
  where go i a as = yield (i, a) >> as


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
  
-- | Drain the output of a 'Producer' and shove it into a monadic 'Bundle'.
drainToMStream :: Monad m => Producer a m () -> Bundle m v a
{-# INLINE drainToMStream #-}
drainToMStream = unfoldrM gen
  where gen producer = do
          state <- next producer
          return $ case state of
           Left _ -> Nothing
           Right x -> Just x


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
-- monadic 'Bundle'.
takeMStream :: Monad m => Int -> Producer a m () -> Bundle m v a
{-# INLINE takeMStream #-}
takeMStream n producer = drainToMStream (producer >-> P.take n) `sized` Max n


