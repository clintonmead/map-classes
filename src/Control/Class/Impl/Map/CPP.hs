{-# LANGUAGE CPP #-}

module Control.Class.Impl.Map.CPP (seqLookup) where

import qualified Data.Sequence
import Data.Maybe.HT (toMaybe)

seqLookup :: Int -> Data.Sequence.Seq a -> Maybe a
#if MIN_VERSION_containers(5,8,0)
seqLookup = Data.Sequence.lookup
#else
seqLookup k x = toMaybe (0 <= k && k < Data.Sequence.length x) (Data.Sequence.index x k)
#endif
