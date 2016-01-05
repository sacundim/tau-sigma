{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Fixed-size sparse maps with `Int` keys, backed by vectors.  The
-- vector will have the same size as the value of the largest key + 1.
-- So this is meant for mostly-dense 'Int'-keyed data sets.
module TauSigma.Util.DenseIntMap
       ( DenseIntMap
       , IntMap
       , UIntMap

       , lookup
       , map
       , mapWithKey
       , filterWithKey
         
       , fromDenseList
       , fromDenseVector
       , toDenseList
       , toDenseVector
       , fromList
       , fromSparseVector
       , toSparseList
       , toSparseVector
       ) where

import Prelude hiding (lookup, map)
import qualified Prelude as P

import Data.Function (on)
import Data.Default

import qualified Data.Foldable as F

import qualified Data.Vector as V
import Data.Vector.Generic (Vector, (!?))
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Data.Vector.Fusion.Stream (Stream)
import qualified Data.Vector.Fusion.Stream as Stream


newtype DenseIntMap v a = DenseIntMap (v (Entry a))

type IntMap a = DenseIntMap V.Vector a
type UIntMap a = DenseIntMap U.Vector a

data Entry a = Entry {-# UNPACK #-} !Bool a


-- We use the default entry to indicate absence.  Dumb hack, but it
-- does allow us below to provide an 'Unbox' instance.
instance Default a => Default (Entry a) where
  def = Entry False def


toEntry :: Default a => Maybe a -> Entry a
{-# INLINE toEntry #-}
toEntry Nothing = Entry False def
toEntry (Just a) = Entry True a

fromEntry :: Entry a -> Maybe a
{-# INLINE fromEntry #-}
fromEntry (Entry False _) = Nothing
fromEntry (Entry True a) = Just a


lookup :: Vector v (Entry a) => Int -> DenseIntMap v a -> Maybe a
{-# INLINE lookup #-}
lookup k (DenseIntMap as) = as!?k >>= fromEntry 

-- | NOTE: your function should be ready to handle the 'Default' values 
map :: (Vector v (Entry a), Vector v (Entry b), Default b) =>
       (a -> b) -> DenseIntMap v a -> DenseIntMap v b
{-# INLINE map #-}
map f (DenseIntMap as) = DenseIntMap (G.map f' as)
  where f' (Entry False _) = def
        f' (Entry True a) = Entry True (f a)

mapWithKey :: (Vector v (Entry a), Vector v (Entry b), Default b) =>
              (Int -> a -> b) -> DenseIntMap v a -> DenseIntMap v b
{-# INLINE mapWithKey #-}
mapWithKey f (DenseIntMap as) = DenseIntMap (G.imap f' as)
  where f' _ (Entry False _) = def
        f' i (Entry True a) = Entry True (f i a)

filterWithKey
  :: (Vector v (Entry a), Default a) =>
     (Int -> a -> Bool) -> DenseIntMap v a -> DenseIntMap v a
filterWithKey p (DenseIntMap as) = DenseIntMap (G.imap p' as)
  where p' _ e@(Entry False _) = e
        p' i e@(Entry True a) 
          | p i a = e
          | otherwise = def
        

--------------------------------------------------------------------
--------------------------------------------------------------------
--
-- Conversion
-- 

fromDenseList
  :: (Vector v (Entry a), Default a) =>
     [Maybe a] -> DenseIntMap v a
fromDenseList as = DenseIntMap (G.fromList (P.map toEntry as))

fromDenseVector
  :: ( Vector v (Maybe a)
     , Vector v' (Entry a)
     , Default a) =>
     v (Maybe a) -> DenseIntMap v' a
fromDenseVector =
  DenseIntMap . G.unstream . Stream.map toEntry . G.stream


-- | This is the same as 'fromSparse' and is provided only for drop-in
-- replacements.
fromList
  :: (Foldable f, Vector v (Entry a), Default a) =>
     f (Int, a) -> DenseIntMap v a
fromList entries = DenseIntMap $ G.create $ do
  let (size, _) = F.maximumBy (compare `on` fst) entries
  result <- M.replicate (size+1) def
  F.forM_ entries $ \(i, a) -> do
    M.write result i (Entry True a)
  return result 

fromSparseVector 
  :: (Vector v (Int, a), Vector v' (Entry a), Default a) =>
     v (Int, a) -> DenseIntMap v' a
fromSparseVector entries = DenseIntMap $ G.create $ do
  let (size, _) = G.maximumBy (compare `on` fst) entries
  result <- M.replicate size def
  G.forM_ entries $ \(i, a) -> do
    M.write result i (Entry True a)
  return result


toDenseList :: Vector v (Entry a) => DenseIntMap v a -> [Maybe a]
toDenseList (DenseIntMap as) = P.map fromEntry (G.toList as)

toDenseVector
  :: (Vector v (Entry a), Vector v' (Maybe a)) =>
     DenseIntMap v a -> v' (Maybe a)
toDenseVector (DenseIntMap as) =
  G.unstream (Stream.map fromEntry (G.stream as))


toSparseList :: Vector v (Entry a) => DenseIntMap v a -> [(Int, a)]
toSparseList (DenseIntMap as) = Stream.toList (toSparseStream (G.stream as))

toSparseVector
  :: (Vector v (Entry a), Vector v' (Int, a)) =>
     DenseIntMap v a -> v' (Int, a)
toSparseVector (DenseIntMap as) = G.unstream (toSparseStream (G.stream as))

toSparseStream :: Stream (Entry a) -> Stream (Int, a)
toSparseStream = Stream.map (\(i, Entry _ a) -> (i, a))
               . Stream.filter isSomething
               . Stream.indexed
  where isSomething (_, Entry b _) = b
    


--------------------------------------------------------------------
--------------------------------------------------------------------
--
-- The following is basically a copy-paste from Data.Vector.Unboxed
-- 

newtype instance U.MVector s (Entry a) = MV_Entry (U.MVector s (Bool, a))
newtype instance U.Vector    (Entry a) = V_Entry  (U.Vector    (Bool, a))

-- | We can only unbox entries for which we know how to construct a
-- \"dummy\" value for @a@.
instance (Default a, U.Unbox a) => U.Unbox (Entry a)

instance U.Unbox a => M.MVector U.MVector (Entry a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Entry v) = M.basicLength v
  basicUnsafeSlice i n (MV_Entry v) = MV_Entry $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Entry v1) (MV_Entry v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Entry <$> M.basicUnsafeNew n
  basicUnsafeReplicate n (Entry x y) =
    MV_Entry <$> M.basicUnsafeReplicate n (x,y)
  basicUnsafeRead (MV_Entry v) i = uncurry Entry <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Entry v) i (Entry x y) = M.basicUnsafeWrite v i (x,y)
  basicClear (MV_Entry v) = M.basicClear v
  basicSet (MV_Entry v) (Entry x y) = M.basicSet v (x,y)
  basicUnsafeCopy (MV_Entry v1) (MV_Entry v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Entry v1) (MV_Entry v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Entry v) n = MV_Entry <$> M.basicUnsafeGrow v n

instance U.Unbox a => G.Vector U.Vector (Entry a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Entry v) = V_Entry <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Entry v) = MV_Entry <$> G.basicUnsafeThaw v
  basicLength (V_Entry v) = G.basicLength v
  basicUnsafeSlice i n (V_Entry v) = V_Entry $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Entry v) i = uncurry Entry <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Entry mv) (V_Entry v) = G.basicUnsafeCopy mv v
  elemseq _ (Entry x y) z = G.elemseq (undefined :: U.Vector Bool) x
                          $ G.elemseq (undefined :: U.Vector a) y z

--
-- The following is basically a copy-paste from Data.Vector.Unboxed
-- 
--------------------------------------------------------------------
--------------------------------------------------------------------

