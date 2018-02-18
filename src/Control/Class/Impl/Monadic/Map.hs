{-|
This module currently is empty. It will eventually contain monadic versions
of the classes in "Control.Class.Impl.Map", but I did a reorganisation
of them so the code here isn't current. There's just a bunch of commented
code below which may or may not be useful when I actually implement this.
-}
module Control.Class.Impl.Monadic.Map where


{-
lookupMMArray :: (Ix i, MArray a e m) => i -> a i e -> m (Maybe e)
lookupMMArray i x = do
  bounds <- Data.Array.MArray.getBounds x
  case Data.Ix.inRange bounds i of
    True -> Just <$> Data.Array.MArray.readArray x i
    False -> pure Nothing

unsafeLookupMMArray :: (Ix i, MArray a e m) => i -> a i e -> m e
unsafeLookupMMArray = flip Data.Array.MArray.readArray

adjustMMArray :: (Ix i, MArray a e m) => (e -> e) -> i -> a i e -> m ()
adjustMMArray f i x = do
  bounds <- Data.Array.MArray.getBounds x
  case (Data.Ix.inRange bounds i) of
    True -> unsafeAdjustMMArray f i x
    False -> pure ()

unsafeAdjustMMArray :: (Ix i, MArray a e m) => (e -> e) -> i -> a i e -> m ()
unsafeAdjustMMArray f i x = do
  curVal <- Data.Array.MArray.readArray x i
  let newVal = f curVal
  Data.Array.MArray.writeArray x i newVal

updateMMArray :: (Ix i, MArray a e m) => i -> e -> a i e -> m ()
updateMMArray i v x = do
  bounds <- Data.Array.MArray.getBounds x
  case (Data.Ix.inRange bounds i) of
    True -> unsafeUpdateMMArray i v x
    False -> pure ()

unsafeUpdateMMArray :: (Ix i, MArray a e m) => i -> e -> a i e -> m ()
unsafeUpdateMMArray i v x = Data.Array.MArray.writeArray x i v

type instance Key (IOArray i _e) = i
type instance Value (IOArray _i e) = e
instance (Ix i, MArray IOArray e IO) => LookupMapM IO (IOArray i e) where
  lookupM = lookupMMArray
  unsafeLookupM = unsafeLookupMMArray
instance (Ix i, MArray IOArray e IO) => UpdateMapM IO (IOArray i e) where
  adjustM = adjustMMArray
  unsafeAdjustM = unsafeAdjustMMArray
  updateM = updateMMArray
  unsafeUpdateM = unsafeUpdateMMArray

type instance Key (IOUArray i _e) = i
type instance Value (IOUArray _i e) = e
instance (Ix i, MArray IOUArray e IO) => LookupMapM IO (IOUArray i e) where
  lookupM = lookupMMArray
instance (Ix i, MArray IOUArray e IO) => UpdateMapM IO (IOUArray i e) where
  adjustM = adjustMMArray
  unsafeAdjustM = unsafeAdjustMMArray

type instance Key (STArray _s i _e) = i
type instance Value (STArray _s _i e) = e
instance (Ix i, MArray (STArray s) e (Strict.ST s)) => LookupMapM (Strict.ST s) (STArray s i e) where
  lookupM = lookupMMArray
instance (Ix i, MArray (STArray s) e (Strict.ST s)) => UpdateMapM (Strict.ST s) (STArray s i e) where
  adjustM = adjustMMArray
  unsafeAdjustM = unsafeAdjustMMArray

type instance Key (STArray _s i _e) = i
type instance Value (STArray _s _i e) = e
instance (Ix i, MArray (STArray s) e (Lazy.ST s)) => LookupMapM (Lazy.ST s) (STArray s i e) where
  lookupM = lookupMMArray
instance (Ix i, MArray (STArray s) e (Lazy.ST s)) => UpdateMapM (Lazy.ST s) (STArray s i e) where
  adjustM = adjustMMArray
  unsafeAdjustM = unsafeAdjustMMArray

type instance Key (STUArray _s i _e) = i
type instance Value (STUArray _s _i e) = e
instance (Ix i, MArray (STUArray s) e (Strict.ST s)) => LookupMapM (Strict.ST s) (STUArray s i e) where
  lookupM = lookupMMArray
instance (Ix i, MArray (STUArray s) e (Strict.ST s)) => UpdateMapM (Strict.ST s) (STUArray s i e) where
  adjustM = adjustMMArray
  unsafeAdjustM = unsafeAdjustMMArray

type instance Key (STUArray _s i _e) = i
type instance Value (STUArray _s _i e) = e
instance (Ix i, MArray (STUArray s) e (Lazy.ST s)) => LookupMapM (Lazy.ST s) (STUArray s i e) where
  lookupM = lookupMMArray
instance (Ix i, MArray (STUArray s) e (Lazy.ST s)) => UpdateMapM (Lazy.ST s) (STUArray s i e) where
  adjustM = adjustMMArray
  unsafeAdjustM = unsafeAdjustMMArray

type instance Key (StorableArray i _e) = i
type instance Value (StorableArray _i e) = e
instance (Ix i, MArray StorableArray e IO) => LookupMapM IO (StorableArray i e) where
  lookupM = lookupMMArray
instance (Ix i, MArray StorableArray e IO) => UpdateMapM IO (StorableArray i e) where
  adjustM = adjustMMArray
  unsafeAdjustM = unsafeAdjustMMArray
-}
