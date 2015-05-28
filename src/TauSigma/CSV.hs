-- | Friendlier wrappers around "Pipes.Csv".
module TauSigma.CSV
       ( decode
       , decodeWith
       , decodeByName
       , decodeByNameWith

       , encode
       , encodeWith
       , encodeByName
       , encodeByNameWith
       ) where

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Data.ByteString (ByteString)
import Data.Either

import Pipes
import Pipes.ByteString (stdin)
import Pipes.Csv hiding (decode, decodeWith, decodeByName, decodeByNameWith)
import qualified Pipes.Csv as C

decode
  :: (Monad m, FromRecord a) =>
     HasHeader
  -> Producer ByteString m ()
  -> Producer a (ExceptT String m) ()
decode hasHeader producer =
  decodeWith defaultDecodeOptions hasHeader producer

decodeWith
  :: (Monad m, FromRecord a) =>
     DecodeOptions
  -> HasHeader
  -> Producer ByteString m ()
  -> Producer a (ExceptT String m) ()
decodeWith options hasHeader producer =
  failOnFirst $ C.decodeWith options hasHeader producer


decodeByName
  :: (Monad m, FromNamedRecord a) =>
     Producer ByteString m () -> Producer a (ExceptT String m) ()
decodeByName producer =
  decodeByNameWith defaultDecodeOptions producer

decodeByNameWith
  :: (Monad m, FromNamedRecord a) =>
     DecodeOptions
  -> Producer ByteString m ()
  -> Producer a (ExceptT String m) ()
decodeByNameWith options producer =
  failOnFirst $ C.decodeByNameWith options producer


failOnFirst
  :: Monad m =>
     Proxy x' x () (Either e a) m r -> Proxy x' x () a (ExceptT e m) r
failOnFirst prod = for (hoist lift prod) $ either (lift . throwE) yield

