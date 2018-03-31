{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-|
If you just want to perform operations on maps, not write your own instances,
"Control.Class.Map" is probably what you should be importing.

This package provides a number of type-classes that encapulate the idea
of a key/value mapping. This includes your standard maps, but also arrays
and potentially hashtables. This library only currently provide instances
for types in package that are distributed with GHC.

Part of the motivation of this library is also consistency.

Pop quiz: Consider the 'Data.Map.Strict.insert', but don't check the documentation.
If the key already exists in the map, which of the following occurs?

1. The map is unchanged.
2. The value at that key is updated.
3. 'error' is called.
4. The result is undefined.

Personally, I had to check the documentation. The answer is actually option "2".

Imagine the potential minefield when changing collection types.

The classes in this library give explicit names for each of these behaviours,
and if the implementers of those instances follow those specifications,
users should be able to switch between different container types without
changing their code nor their code's behaviour.

The naming convention and argument order is somewhat arbitary.

I've tried to follow existing convention but the existing convention is a bit mixed up.

For example 'Data.Map.Strict.insert' for maps is actually called 'upsert' in this library
because that's what it actually does.

In anycase, I'll attempt to define the broad naming convention here, but there
are further details in each class.

There's a number of prefixes to function which affect expected behaviour.

1. The unprefixed functions should call 'error' if something is unexpected,
   e.g. a key already exists on 'insert' or a key is not in collection on 'delete'.
   They must not just return the structure unchanged, that is the role of 'maybe'
   prefixed functions.
2. The "unsafe" prefixed functions may optionally just behave in an undefined fashion
   in the above case where one would instead 'error'. For example, 'unsafe'
   functions may do array lookups without bounds checking, potentially resulting
   in demons if they access memory they shouldn't.
3. The "maybe" prefixed functions shall not call 'error' if the operation can
   not be completed but instead return the structure unchanged.
4. The "safe" prefixed functions actually have a 'Maybe' return type which indicate
   whether the key is not found/already exists on insert.

Functions suffixed with "Lookup" actually have a different return type and
generally allow one to access the contents of the structure before the change,
the exact form depending on the function in particular. The reason for the
"Lookup" suffix is that to implement these naively one can do a lookup before
performing the operation. However, for example with 'deleteLookup' on a map,
it would be more efficient to just lookup the element to delete, grab it and delete
it at the same time, so there is a point in overriding the default implementation.

Finally, you may notice some of the class functions that ordinarily accept a 'Functor',
are renamed ending with a @..F_@, and now have the 'Functor' wrapped in a 'Coyoneda'.
This is because having 'Functor's in class function defintions
does not work with generalised newtype deriving.

The versions of the functions without the following underscores, i.e. @..F@
are what users should be using. When defining your own instances for these
functions, it's probably best just apply 'toCoyonedaTransform'/'toCoyonedaTransformF'
to their ordinary definitions. The non underscore style defintions run
'fromCoyonedaTransform'/'fromCoyonedaTransformF' on the class functions.
Ideally rewrite rules included in these modules should reduce this pair of
functions to 'id' resulting in no runtime difference.

Regarding trailing @F@ on the latter 'toCoyonedaTransform'/'toCoyonedaTransformF'
function, use that when defining such 'Coyondea' class functions which have
return types wrapped in 'Maybe', namely the ones prefixed with @safe...@.

To Do: Monadic versions of these functions, to be used on mutable structures for example.

Also To Do: Range lookups (and perhaps even range deletes?). In theory, for say maps,
range lookups are not only possible but also faster than accessing the keys individually.
But they've impossible for say hashmaps.

Pull requests welcome on github.
-}

module Control.Class.Impl.Map (
  Key, Value,
  LookupMap(..),
  SingletonMap(..),
  InsertMap(..),
  UpdateMap(..), adjustF, unsafeAdjustF, safeAdjustF,
  DeleteMap(..), optDeleteF, unsafeOptDeleteF, safeOptDeleteF,
  UpsertMap(..), adsertF,
  UpleteMap(..), adleteF, unsafeAdleteF, safeAdleteF,
  AlterMap(..), alterF,
  Strict(..), Lazy(..),
  (!),
  fromCoyonedaTransform, fromCoyonedaTransformF,
  toCoyonedaTransform, toCoyonedaTransformF,
  ) where

import qualified Data.Map.Strict
import qualified Data.Map.Lazy
import qualified Data.IntMap.Strict
import qualified Data.IntMap.Lazy
import qualified Data.Set
import Data.Set (Set)
import qualified Data.IntSet
import Data.IntSet (IntSet)
import qualified Data.Sequence
import Data.Sequence (Seq)
import Data.Ix (Ix)
import qualified Data.Array.IArray

import Prelude hiding (lookup)
import qualified Control.Class.Impl.Map.CPP

import Data.Maybe (fromMaybe, isJust)

import Data.Functor.Identity (Identity(Identity, runIdentity))

import Data.Functor.Compose (Compose(Compose, getCompose))

import Data.Maybe.HT (toMaybe)

import Data.Coerce (Coercible, coerce)
import Data.Functor.Coyoneda (Coyoneda, liftCoyoneda, lowerCoyoneda)

import Data.Array (Array)

import qualified Data.ByteString
import qualified Data.ByteString.Unsafe
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short

import Data.Word (Word8)
import Data.Int (Int64)

{-# ANN module "HLint: ignore Use if" #-}

type family Key t
type family Value t

{-| Hack to allow generalised newtype deriving from https://stackoverflow.com/questions/48848571/generalised-newtype-deriving-on-class-functions-with-functors/48849568#48849568 -}
{-# INLINE[1] fromCoyonedaTransform #-}
fromCoyonedaTransform :: Functor f1 =>
            ((a1 -> Coyoneda f2 a2) -> t1 -> t2 -> Coyoneda f1 a3)
            -> (a1 -> f2 a2) -> t1 -> t2 -> f1 a3
fromCoyonedaTransform g f k x = lowerCoyoneda $ g (liftCoyoneda . f) k x

{-# INLINE[1] fromCoyonedaTransformF #-}
fromCoyonedaTransformF :: (Functor f1, Functor f3) =>
            ((a1 -> Coyoneda f2 a2) -> t1 -> t2 -> f3 (Coyoneda f1 a3))
            -> (a1 -> f2 a2) -> t1 -> t2 -> f3 (f1 a3)
fromCoyonedaTransformF g f k x = lowerCoyoneda <$> g (liftCoyoneda . f) k x

{-# INLINE[1] toCoyonedaTransform #-}
toCoyonedaTransform :: Functor f =>
            (forall f'. Functor f' => (a1 -> f' a2) -> t1 -> t2 -> f' a3)
            -> ((a1 -> Coyoneda f a2) -> t1 -> t2 -> Coyoneda f a3)
toCoyonedaTransform = id

{-# INLINE[1] toCoyonedaTransformF #-}
toCoyonedaTransformF :: Functor f =>
            (forall f'. Functor f' => (a1 -> f' a2) -> t1 -> t2 -> f3 (f' a3))
            -> ((a1 -> Coyoneda f a2) -> t1 -> t2 -> f3 (Coyoneda f a3))
toCoyonedaTransformF = id


{-# RULES
-- An attempt to remove going to and from Coyonedas.
"fromToCoyonedaTransform"  forall (x :: forall f2' f1'. (a1 -> f2' a2) -> t1 -> t2 -> f1' a3). fromCoyonedaTransform (toCoyonedaTransform x) = x
"fromToCoyonedaTransformF" forall (x :: forall f2' f1'. (a1 -> f2' a2) -> t1 -> t2 -> f3 (f1' a3)). fromCoyonedaTransformF (toCoyonedaTransformF x) = x
-- How do I write these rules? Should I even write these rules?
-- "fromToCoyonedaTransform"  fromCoyonedaTransform . toCoyonedaTransform = id
-- "fromToCoyonedaTransformF" fromCoyonedaTransformF . toCoyonedaTransformF = id
#-}
{-|
'LookupMap' is a class that simply represents data types indexable by a key that
you can read from. Whilst obviously not enforced by the class, it's intended that
this only be implemented for types with "fast" lookups, say O(log n) at most.

Hence, 'LookupMap' is not implemented for list for example.

Not that 'Data.Set.Set' is an instance of this type, where the keys are just the
set values and the unit type '()' is the "value" type.

You could in theory implement 'LookupMap'
(and indeed associated classes like 'UpdateMap' and 'AlterMap') for structures with
multiple keys, by making the key type a sum type or a list or something.
-}
class LookupMap t where
  {-# MINIMAL lookup | ((unsafeIndex | index), member) #-}

  {-| @lookup k x@ returns @Just v@ if @k@ is a key, @Nothing@ otherwise -}
  lookup :: Key t -> t -> Maybe (Value t)
  lookup k x = case member k x of
    True -> Just (unsafeIndex k x)
    False -> Nothing

  {-| Like 'lookup' but throws an error for values that don't exist -}
  index :: Key t -> t -> Value t
  index k x = fromMaybe (error "index: Key does not exist.") (lookup k x)

  {-| Like 'index' but may be undefined for keys that don't exist -}
  unsafeIndex :: Key t -> t -> Value t
  unsafeIndex = index

  member :: Key t -> t -> Bool
  member k x = isJust (lookup k x)

  notMember :: Key t -> t -> Bool
  notMember k x = not (member k x)

{-|
Data types you can produce a one element container of.

The reason why this is a separate class instead of just the default instance
is that there are contrainers where one can trivially make a singleton of
but they're not 'Monoid's or 'AlterMap's, i.e. you can't append or add elements to them
at arbitary keys.

For example, arrays certainly don't have the concept of "insert at key", only update,
nor is it obvious how to append them, particularly if their ranges overlap.

But given a key, one should be able to produce a singleton array.

Hence this class.
-}
class LookupMap t => SingletonMap t where
  singleton :: Key t -> Value t -> t
--  default singleton :: (Monoid t, AlterMap t) => Key t -> Value t -> t
--  singleton k v =  insert k v mempty

{-|
'UpdateMap' represents types where existing values can be updated.

The ability for keys to be inserted or deleted is optional.

A good example of a type which conforms to this is 'Data.Sequence.Seq', which
has 'Int' keys of which their values can be updated in "O(log n)" time.

However 'Data.Sequence.Seq' is not an instance of 'AlterMap' as although
one can insert/delete from 'Data.Sequence.Seq' it alters all the other indexes
which would be very unexpected.
-}
class LookupMap t => UpdateMap t where
  {-# MINIMAL unsafeUpdate | update | safeUpdate | safeUpdateLookup | safeAdjustLookup | safeAdjustLookup | safeAdjustF_ #-}

  {-| Updates the value of a key, calls 'error' if the key does not exist. -}
  update :: Key t -> Value t -> t -> t
  update k v x = fromMaybe (error "update: Key not found.") (safeUpdate k v x)

  updateLookup :: Key t -> Value t -> t -> (Value t, t)
  updateLookup k v x = fromMaybe (error "updateLookup: Key not found.") (safeUpdateLookup k v x)

  {-| Like 'update', but if the key does not exist the result is undefined. -}
  unsafeUpdate :: Key t -> Value t -> t -> t
  unsafeUpdate = update

  unsafeUpdateLookup :: Key t -> Value t -> t -> (Value t, t)
  unsafeUpdateLookup = updateLookup

  maybeUpdate :: Key t -> Value t -> t -> t
  maybeUpdate k v x = fromMaybe x (safeUpdate k v x)

  safeUpdate :: Key t -> Value t -> t -> Maybe t
  safeUpdate k v x = snd <$> safeUpdateLookup k v x

  safeUpdateLookup :: Key t -> Value t -> t -> Maybe (Value t, t)
  safeUpdateLookup k v = safeAdjustLookup g k where
    g old_v = (old_v, v)

  {-|
  @adjust f k x@ applies @f@ to the value at key @k@
  and puts that modified value in it's place.

  If the key does not exist it should throw an error.
  -}
  adjust :: (Value t -> Value t) -> Key t -> t -> t
  adjust f k x = fromMaybe (error "Adjust: Key not found.") (safeAdjust f k x)

  adjustLookup :: (Value t -> (r, Value t)) -> Key t -> t -> (r, t)
  adjustLookup f k x = fromMaybe (error "AdjustLookup: Key not found.") (safeAdjustLookup f k x)

  adjustF_ :: Functor f => (Value t -> Coyoneda f (Value t)) -> Key t -> t -> Coyoneda f t
  adjustF_ f k x = fromMaybe (error "AdjustF: Key not found.") (safeAdjustF_ f k x)

  unsafeAdjust :: (Value t -> Value t) -> Key t -> t -> t
  unsafeAdjust f k x = runIdentity $ unsafeAdjustF (Identity . f) k x

  unsafeAdjustLookup :: (Value t -> (r, Value t)) -> Key t -> t -> (r, t)
  unsafeAdjustLookup = unsafeAdjustF

  unsafeAdjustF_ :: Functor f => (Value t -> Coyoneda f (Value t)) -> Key t -> t -> Coyoneda f t
  unsafeAdjustF_ = adjustF_

  maybeAdjust :: (Value t -> Value t) -> Key t -> t -> t
  maybeAdjust f k x = fromMaybe x (safeAdjust f k x)

  safeAdjust :: (Value t -> Value t) -> Key t -> t -> Maybe t
  safeAdjust f k x = runIdentity <$> safeAdjustF (Identity . f) k x

  safeAdjustLookup :: (Value t -> (r, Value t)) -> Key t -> t -> Maybe (r, t)
  safeAdjustLookup = safeAdjustF

  safeAdjustF_ :: Functor f => (Value t -> Coyoneda f (Value t)) -> Key t -> t -> Maybe (Coyoneda f t)
  default safeAdjustF_ :: (UpsertMap t, Functor f) => (Value t -> Coyoneda f (Value t)) -> Key t -> t -> Maybe (Coyoneda f t)
  safeAdjustF_ = defaultSafeAdjustFBasedOnAdsertF

unsafeAdjustF :: (UpdateMap t, Functor f) => (Value t -> f (Value t)) -> Key t -> t -> f t
unsafeAdjustF = fromCoyonedaTransform unsafeAdjustF_

adjustF :: (UpdateMap t, Functor f) => (Value t -> f (Value t)) -> Key t -> t -> f t
adjustF = fromCoyonedaTransform adjustF_

safeAdjustF :: (UpdateMap t, Functor f) => (Value t -> f (Value t)) -> Key t -> t -> Maybe (f t)
safeAdjustF = fromCoyonedaTransformF safeAdjustF_

defaultSafeAdjustFBasedOnAdsertF :: (UpsertMap t, Functor f) => (Value t -> f (Value t)) -> Key t -> t -> Maybe (f t)
defaultSafeAdjustFBasedOnAdsertF f k x = getCompose $ adsertF (Compose . fmap f) k x

defaultSafeAdjustFBasedOnUnsafeUpdate :: (UpdateMap t, Functor f) => (Value t -> f (Value t)) -> Key t -> t -> Maybe (f t)
defaultSafeAdjustFBasedOnUnsafeUpdate f k x = g <$> lookup k x where
  g old_val =
    let
      new_x_func new_val = unsafeUpdate k new_val x
    in
      new_x_func <$> f old_val
{-|
'InsertMap' represents types where new key-values pairs can be inserted.
-}
class LookupMap t => InsertMap t where
  {-# MINIMAL unsafeInsert | insert | safeInsert #-}
  {-|
  Attempts to insert a value, calls 'error' if the key already exists.
  -}
  insert :: Key t -> Value t -> t -> t
  insert k v x = fromMaybe (error "Insert: Key already exists.") (safeInsert k v x)

  {-|
  Like 'insert', but if the key already exists the behaviour is undefined.
  -}
  unsafeInsert :: Key t -> Value t -> t -> t
  unsafeInsert = insert

  {-|
  Like 'insert', but if the key already exists return the structure unchanged.
  -}
  maybeInsert :: Key t -> Value t -> t -> t
  maybeInsert k v x = fromMaybe x (safeInsert k v x)

  {-|
  Like 'insert', but if the key already exists return 'Nothing'.
  -}
  safeInsert :: Key t -> Value t -> t -> Maybe t
  default safeInsert :: UpsertMap t => Key t -> Value t -> t -> Maybe t
  safeInsert = defaultSafeInsertBasedOnAdsertF

defaultSafeInsertBasedOnAdsertF :: UpsertMap t => Key t -> Value t -> t -> Maybe t
defaultSafeInsertBasedOnAdsertF k v = adsertF (fmap (const v)) k

{-|
'DeleteMap' represents types where keys can be deleted.
-}
class LookupMap t => DeleteMap t where
  {-# MINIMAL unsafeDelete | delete | safeDelete | safeDeleteLookup #-}

  {-| Attempt to delete a key and call 'error' if it's not found. -}
  delete :: Key t -> t -> t
  delete k x = fromMaybe (error "delete: key not found.") (safeDelete k x)

  {-| Like 'delete', but also return the value at the key before deletion. -}
  deleteLookup :: Key t -> t -> (Value t, t)
  deleteLookup k x = fromMaybe (error "deleteLookup: key not found.") (safeDeleteLookup k x)

  {-| Like 'delete' but if the key isn't found the result is undefined -}
  unsafeDelete :: Key t -> t -> t
  unsafeDelete = delete

  {-| Like 'deleteLookup' but if the key isn't found the result is undefined -}
  unsafeDeleteLookup :: Key t -> t -> (Value t, t)
  unsafeDeleteLookup = deleteLookup

  {-|  Like 'delete', but return the structure unmodified if the key does not exist. -}
  maybeDelete :: Key t -> t -> t
  maybeDelete k x = fromMaybe x (safeDelete k x)

  {-| Like 'delete', but return 'Nothing' the key does not exist. -}
  safeDelete :: Key t -> t -> Maybe t
  safeDelete k x = snd <$> safeDeleteLookup k x

  {-| Like 'safeDelete', but also return the value of the key before the delete. -}
  safeDeleteLookup :: Key t -> t -> Maybe (Value t, t)
  safeDeleteLookup = safeOptDeleteLookup g where
    g val = (val, True)

  {-| Attempt to optDelete a key based on it's value and call 'error' if it's not found. -}
  optDelete :: (Value t -> Bool) -> Key t -> t -> t
  optDelete f k x = fromMaybe (error "optDelete: key not found.") (safeOptDelete f k x)

  {-| Like 'optDelete', but also return the value at the key before deletion. -}
  optDeleteLookup :: (Value t -> (r, Bool)) -> Key t -> t -> (r, t)
  optDeleteLookup f k x = fromMaybe (error "optDeleteLookup: key not found.") (safeOptDeleteLookup f k x)

  optDeleteF_ :: Functor f => (Value t -> Coyoneda f Bool) -> Key t -> t -> Coyoneda f t
  optDeleteF_ f k x = fromMaybe (error "optDeleteF: key not found.") (safeOptDeleteF f k x)

  {-| Like 'optDelete' but if the key isn't found the result is undefined -}
  unsafeOptDelete :: (Value t -> Bool) -> Key t -> t -> t
  unsafeOptDelete f k x = runIdentity $ unsafeOptDeleteF (Identity . f) k x

  {-| Like 'optDeleteLookup' but if the key isn't found the result is undefined -}
  unsafeOptDeleteLookup :: (Value t -> (r, Bool)) -> Key t -> t -> (r, t)
  unsafeOptDeleteLookup = unsafeOptDeleteF

  unsafeOptDeleteF_ :: Functor f => (Value t -> Coyoneda f Bool) -> Key t -> t -> Coyoneda f t
  unsafeOptDeleteF_ = optDeleteF

  {-| Like 'optDelete', but return the structure unmodified if the key does not exist. -}
  maybeOptDelete :: (Value t -> Bool) -> Key t -> t -> t
  maybeOptDelete f k x = fromMaybe x (safeOptDelete f k x)

  {-| Like 'optDelete', but return 'Nothing' the key does not exist. -}
  safeOptDelete :: (Value t -> Bool) -> Key t -> t -> Maybe t
  safeOptDelete f k x = runIdentity <$> safeOptDeleteF (Identity . f) k x

  {-| Like 'safeOptDelete', but also return the value of the key before the optDelete. -}
  safeOptDeleteLookup :: (Value t -> (r, Bool)) -> Key t -> t -> Maybe (r, t)
  safeOptDeleteLookup = safeOptDeleteF

  safeOptDeleteF_ :: Functor f => (Value t -> Coyoneda f Bool) -> Key t -> t -> Maybe (Coyoneda f t)
  default safeOptDeleteF_ :: (UpleteMap t, Functor f) => (Value t -> Coyoneda f Bool) -> Key t -> t -> Maybe (Coyoneda f t)
  safeOptDeleteF_ = defaultOptDeleteFBasedOnSafeAdleteF


unsafeOptDeleteF :: (DeleteMap t, Functor f) => (Value t -> f Bool) -> Key t -> t -> f t
unsafeOptDeleteF = fromCoyonedaTransform unsafeOptDeleteF_

optDeleteF :: (DeleteMap t, Functor f) => (Value t -> f Bool) -> Key t -> t -> f t
optDeleteF = fromCoyonedaTransform optDeleteF_

safeOptDeleteF :: (DeleteMap t, Functor f) => (Value t -> f Bool) -> Key t -> t -> Maybe (f t)
safeOptDeleteF = fromCoyonedaTransformF safeOptDeleteF_

defaultOptDeleteFBasedOnSafeAdleteF :: (UpleteMap t, Functor f) => (Value t -> f Bool) -> Key t -> t -> Maybe (f t)
defaultOptDeleteFBasedOnSafeAdleteF f = safeAdleteF g where
  g val = (`toMaybe` val) <$> f val

{-|
Functions for doing inserts that don't fail on the keys being found
but instead override existing values.
-}
class (InsertMap t, UpdateMap t) => UpsertMap t where
  upsert :: Key t -> Value t -> t -> t
  upsert k v x = snd (upsertLookup k v x)

  upsertLookup :: Key t -> Value t -> t -> (Maybe (Value t), t)
  upsertLookup k v = adsertLookup g k where
    g old_v = (old_v, v)

  adsert :: (Maybe (Value t) -> Value t) -> Key t -> t -> t
  adsert f k x = snd $ adsertLookup g k x where
    g maybe_old_v = ((), f maybe_old_v)

  adsertLookup :: (Maybe (Value t) -> (r, Value t)) -> Key t -> t -> (r, t)
  adsertLookup = adsertF

  adsertF_ :: Functor f => (Maybe (Value t) -> Coyoneda f (Value t)) -> Key t -> t -> Coyoneda f t
  default adsertF_ :: (AlterMap t, Functor f) => (Maybe (Value t) -> Coyoneda f (Value t)) -> Key t -> t -> Coyoneda f t
  adsertF_ = defaultAdsertFBasedOnAlterF

adsertF :: (UpsertMap t, Functor f) => (Maybe (Value t) -> f (Value t)) -> Key t -> t -> f t
adsertF = fromCoyonedaTransform adsertF_

defaultAdsertFBasedOnAlterF :: (AlterMap t, Functor f) => (Maybe (Value t) -> f (Value t)) -> Key t -> t -> f t
defaultAdsertFBasedOnAlterF f = alterF (fmap Just . f)

class (DeleteMap t, UpdateMap t) => UpleteMap t where
  adlete :: (Value t -> Maybe (Value t)) -> Key t -> t -> t
  adlete f k x = fromMaybe (error "Adlete: Key not found.") (safeAdlete f k x)

  adleteLookup :: (Value t -> (r, Maybe (Value t))) -> Key t -> t -> (r, t)
  adleteLookup f k x = fromMaybe (error "AdleteLookup: Key not found.") (safeAdleteLookup f k x)

  adleteF_ :: Functor f => (Value t -> Coyoneda f (Maybe (Value t))) -> Key t -> t -> Coyoneda f t
  adleteF_ f k x = fromMaybe (error "AdleteF: Key not found.") (safeAdleteF_ f k x)

  unsafeAdlete :: (Value t -> Maybe (Value t)) -> Key t -> t -> t
  unsafeAdlete f k x = runIdentity $ unsafeAdleteF (Identity . f) k x

  unsafeAdleteLookup :: (Value t -> (r, Maybe (Value t))) -> Key t -> t -> (r, t)
  unsafeAdleteLookup = unsafeAdleteF

  unsafeAdleteF_ :: Functor f => (Value t -> Coyoneda f (Maybe (Value t))) -> Key t -> t -> Coyoneda f t
  unsafeAdleteF_ = adleteF

  maybeAdlete :: (Value t -> Maybe (Value t)) -> Key t -> t -> t
  maybeAdlete f k x = fromMaybe x (safeAdlete f k x)

  safeAdlete :: (Value t -> Maybe (Value t)) -> Key t -> t -> Maybe t
  safeAdlete f k x = runIdentity <$> safeAdleteF (Identity . f) k x

  safeAdleteLookup :: (Value t -> (r, Maybe (Value t))) -> Key t -> t -> Maybe (r, t)
  safeAdleteLookup = safeAdleteF

  safeAdleteF_ :: Functor f => (Value t -> Coyoneda f (Maybe (Value t))) -> Key t -> t -> Maybe (Coyoneda f t)
  default safeAdleteF_ :: (AlterMap t, Functor f) => (Value t -> Coyoneda f (Maybe (Value t))) -> Key t -> t -> Maybe (Coyoneda f t)
  safeAdleteF_ = defaultSafeAdleteFBasedOnAlterF

safeAdleteF :: (UpleteMap t, Functor f) => (Value t -> f (Maybe (Value t))) -> Key t -> t -> Maybe (f t)
safeAdleteF = fromCoyonedaTransformF safeAdleteF_

unsafeAdleteF :: (UpleteMap t, Functor f) => (Value t -> f (Maybe (Value t))) -> Key t -> t -> f t
unsafeAdleteF = fromCoyonedaTransform unsafeAdleteF_

adleteF :: (UpleteMap t, Functor f) => (Value t -> f (Maybe (Value t))) -> Key t -> t -> f t
adleteF = fromCoyonedaTransform adleteF_

defaultSafeAdleteFBasedOnAlterF :: (AlterMap t, Functor f) => (Value t -> f (Maybe (Value t))) -> Key t -> t -> Maybe (f t)
defaultSafeAdleteFBasedOnAlterF f k x = getCompose $ alterF (Compose . fmap f) k x

{-|
'AlterMap' is a class that represents key-value mappings where one can do
inserts, deletes, updates, pretty much everything you expect from a simple
key/value store.
-}
class (UpsertMap t, UpleteMap t) => AlterMap t where
  {-|
  @alter f k x@ attempts to gets the value of the key @k@.

  If key @k@ exists, as say it is @v@, it passes @Just v@ to @f@.

  If key @k@ does not exist, it passes @Nothing@ to @f@.

  If the result of @f@ is @Just something@, then 'alter' either inserts or updates
  the key @k@, inserting if key @k@ previously didn't exist and updating if it did.

  If the result of @f@ is @Nothing@, and the key @k@ did exist, we deleted it.

  Otherwise,  if the result of @f@ is @Nothing@, nd the key @k@ did not exist,
  then do nothing and simply return the structure unmodified.
  -}
  alter :: (Maybe (Value t) -> Maybe (Value t)) -> Key t -> t -> t
  alter f k x = let g v = ((), f v) in snd (alterLookup g k x)

  {-|
  Like 'alter', but returns the value both before and after the alteration.
  -}
  alterLookup :: (Maybe (Value t) -> (r, Maybe (Value t))) -> Key t -> t -> (r, t)
  alterLookup = alterF

  alterF_ :: Functor f => (Maybe (Value t) -> Coyoneda f (Maybe (Value t))) -> Key t -> t -> Coyoneda f t
  alterF_ = defaultAlterFBasedOnUnsafeInsertUpdateDelete

alterF :: (AlterMap t, Functor f) => (Maybe (Value t) -> f (Maybe (Value t))) -> Key t -> t -> f t
alterF = fromCoyonedaTransform alterF_

defaultAlterFBasedOnUnsafeInsertUpdateDelete :: (InsertMap t, UpdateMap t, DeleteMap t, Functor f) => (Maybe (Value t) -> f (Maybe (Value t))) -> Key t -> t -> f t
defaultAlterFBasedOnUnsafeInsertUpdateDelete f k x =
 let
   maybe_old_val = lookup k x

   new_x_func = case maybe_old_val of
     Nothing -> \maybe_new_val -> case maybe_new_val of
       Nothing -> x
       Just new_val -> unsafeInsert k new_val x
     Just _ -> \maybe_new_val -> case maybe_new_val of
       Nothing -> unsafeDelete k x
       Just new_val -> unsafeUpdate k new_val x
 in
   new_x_func <$> f maybe_old_val


{-|
'AppendMap' is a class describing key-value stores where one can
add a value to container without giving a key, and the container will
automatically generate a key that doesn't exist in the container.

'Data.Sequence.Seq' is a good example of a structure with this ability.

Again, it's intended for this to only be defined when the operation is "fast",
say "O(log n)" on average or less.
-}
class LookupMap t => AppendMap t where
  {-|
  @appendGetKey v x@ adds the value @v@ to @x@ and returns both the
  updated @x@ and the new key @k@ selected.
  -}
  appendGetKey :: Value t -> t -> (Key t, t)
  {-|
  Like 'appendGetKey' but don't worry about returning the key.
  -}
  append :: Value t -> t -> t
  append v x = snd (appendGetKey v x)

{-|
For certain types like maps in the standard containers library that ships with GHC,
the strict version of the data type: 'Data.Map.Strict.Map',
and the lazy version of the data type: 'Data.Map.Lazy.Map',
are actually the exact same type. In this case, they're just reexports of the
same type.

That's fine when one has two separate modules with strict and lazy versions
one can explicitly use, but the choice can't be automatic based on the type.

As a result, there's no way one can tell whether to use strict or
lazy functions on the data. Wrapping these types in either 'Strict' or 'Lazy'
specifies how these types are intend to be worked on.

By default however, if one doesn't wrap, the 'Strict' version is used.
-}
newtype Strict t = Strict { getStrict :: t }

{-|
See 'Strict' documentation for a discussion of the 'Lazy' wrapper.
-}
newtype Lazy t = Lazy { getLazy :: t }

class IsStrictMap t

class IsLazyMap t

type instance Key   (Strict t) = Key t
type instance Value (Strict t) = Value t

type instance Key   (Lazy t) = Key t
type instance Value (Lazy t) = Value t

instance IsStrictMap t => IsStrictMap (Strict t)

deriving instance {-# OVERLAPPABLE #-} (IsStrictMap t, LookupMap t) => LookupMap (Strict t)
deriving instance {-# OVERLAPPABLE #-} (IsStrictMap t, InsertMap t) => InsertMap (Strict t)
deriving instance {-# OVERLAPPABLE #-} (IsStrictMap t, UpdateMap t) => UpdateMap (Strict t)
deriving instance {-# OVERLAPPABLE #-} (IsStrictMap t, DeleteMap t) => DeleteMap (Strict t)
deriving instance {-# OVERLAPPABLE #-} (IsStrictMap t, UpsertMap t) => UpsertMap (Strict t)
deriving instance {-# OVERLAPPABLE #-} (IsStrictMap t, UpleteMap t) => UpleteMap (Strict t)
deriving instance {-# OVERLAPPABLE #-} (IsStrictMap t, AlterMap  t) => AlterMap  (Strict t)

type instance Key   (Lazy t) = Key t
type instance Value (Lazy t) = Value t

instance IsLazyMap t => IsLazyMap (Lazy t)

deriving instance {-# OVERLAPPABLE #-} (IsLazyMap t, LookupMap t) => LookupMap (Lazy t)
deriving instance {-# OVERLAPPABLE #-} (IsLazyMap t, InsertMap t) => InsertMap (Lazy t)
deriving instance {-# OVERLAPPABLE #-} (IsLazyMap t, UpdateMap t) => UpdateMap (Lazy t)
deriving instance {-# OVERLAPPABLE #-} (IsLazyMap t, DeleteMap t) => DeleteMap (Lazy t)
deriving instance {-# OVERLAPPABLE #-} (IsLazyMap t, UpsertMap t) => UpsertMap (Lazy t)
deriving instance {-# OVERLAPPABLE #-} (IsLazyMap t, UpleteMap t) => UpleteMap (Lazy t)
deriving instance {-# OVERLAPPABLE #-} (IsLazyMap t, AlterMap  t) => AlterMap  (Lazy t)

unwrapCoerce1 :: (Coercible (f t2) t2) => (t1 -> t2 -> t3) -> t1 -> f t2 -> t3
unwrapCoerce1 f = g where
  g x1 x2 = f x1 (coerce x2)

rewrapCoerce1 :: (Coercible (f t2) t2, Coercible t3 (f t3)) => (t1 -> t2 -> t3) -> t1 -> f t2 -> f t3
rewrapCoerce1 f = g where
  g x1 x2 = coerce (f x1 (coerce x2))

rewrapCoerce2 :: (Coercible (f t3) t3, Coercible t4 (f t4)) => (t1 -> t2 -> t3 -> t4) -> t1 -> t2 -> f t3 -> f t4
rewrapCoerce2 f = g where
  g x1 x2 x3 = coerce (f x1 x2 (coerce x3))

rewrapCoerce2F :: (Coercible (f t3) t3, Coercible t4 (f t4), Functor g) => (t1 -> t2 -> t3 -> g t4) -> t1 -> t2 -> f t3 -> g (f t4)
rewrapCoerce2F f = g where
  g x1 x2 x3 = coerce <$> f x1 x2 (coerce x3)

type instance Key   (Data.Map.Strict.Map k _) = k
type instance Value (Data.Map.Strict.Map _ v) = v
instance IsStrictMap (Data.Map.Strict.Map k v)

instance Ord k => SingletonMap (Data.Map.Strict.Map k v) where
  singleton = Data.Map.Strict.singleton
instance Ord k => LookupMap (Data.Map.Strict.Map k v) where
  lookup = Data.Map.Strict.lookup
  index = flip (Data.Map.Strict.!)
  member = Data.Map.Strict.member
  notMember = Data.Map.Strict.notMember
instance Ord k => InsertMap (Data.Map.Strict.Map k v) where
  unsafeInsert = Data.Map.Strict.insert
instance Ord k => UpdateMap (Data.Map.Strict.Map k v) where
  unsafeUpdate = Data.Map.Strict.insert
  unsafeAdjust = Data.Map.Strict.adjust
  maybeAdjust = Data.Map.Strict.adjust
instance Ord k => DeleteMap (Data.Map.Strict.Map k v) where
  unsafeDelete = Data.Map.Strict.delete
  maybeDelete = Data.Map.Strict.delete
instance Ord k => UpsertMap (Data.Map.Strict.Map k v) where
  upsert = Data.Map.Strict.insert
instance Ord k => UpleteMap (Data.Map.Strict.Map k v) where
  adlete = Data.Map.Strict.update
instance Ord k => AlterMap  (Data.Map.Strict.Map k v) where
  alter = Data.Map.Strict.alter
  alterF_ = toCoyonedaTransform Data.Map.Strict.alterF

instance Ord k => LookupMap (Lazy (Data.Map.Lazy.Map k v)) where
  lookup = unwrapCoerce1 Data.Map.Lazy.lookup
  index = unwrapCoerce1 $ flip (Data.Map.Lazy.!)
  member = unwrapCoerce1 Data.Map.Lazy.member
  notMember = unwrapCoerce1 Data.Map.Lazy.notMember
instance Ord k => SingletonMap (Lazy (Data.Map.Lazy.Map k v)) where
  singleton k v = Lazy (Data.Map.Lazy.singleton k v)
instance Ord k => InsertMap (Lazy (Data.Map.Lazy.Map k v)) where
  unsafeInsert = rewrapCoerce2 Data.Map.Lazy.insert
instance Ord k => UpdateMap (Lazy (Data.Map.Lazy.Map k v)) where
  unsafeUpdate = rewrapCoerce2 Data.Map.Lazy.insert
  unsafeAdjust = rewrapCoerce2 Data.Map.Lazy.adjust
  maybeAdjust = rewrapCoerce2 Data.Map.Lazy.adjust
instance Ord k => DeleteMap (Lazy (Data.Map.Lazy.Map k v)) where
  unsafeDelete = rewrapCoerce1 Data.Map.Lazy.delete
  maybeDelete = rewrapCoerce1 Data.Map.Lazy.delete
instance Ord k => UpsertMap (Lazy (Data.Map.Lazy.Map k v)) where
  upsert = rewrapCoerce2 Data.Map.Lazy.insert
instance Ord k => UpleteMap (Lazy (Data.Map.Lazy.Map k v)) where
  adlete = rewrapCoerce2 Data.Map.Lazy.update
instance Ord k => AlterMap  (Lazy (Data.Map.Lazy.Map k v)) where
  alter = rewrapCoerce2 Data.Map.Lazy.alter
  alterF_ = toCoyonedaTransform (rewrapCoerce2F Data.Map.Lazy.alterF)

type instance Key (Data.IntMap.Strict.IntMap v) = Int
type instance Value (Data.IntMap.Strict.IntMap v) = v
instance IsStrictMap (Data.IntMap.Strict.IntMap v)

instance LookupMap (Data.IntMap.Strict.IntMap v) where
  lookup = Data.IntMap.Strict.lookup
  index = flip (Data.IntMap.Strict.!)
  member = Data.IntMap.Strict.member
  notMember = Data.IntMap.Strict.notMember
instance SingletonMap (Data.IntMap.Strict.IntMap v) where
  singleton = Data.IntMap.Strict.singleton
instance InsertMap (Data.IntMap.Strict.IntMap v) where
  unsafeInsert = Data.IntMap.Strict.insert
instance UpdateMap (Data.IntMap.Strict.IntMap v) where
  unsafeUpdate = Data.IntMap.Strict.insert
  unsafeAdjust = Data.IntMap.Strict.adjust
  maybeAdjust = Data.IntMap.Strict.adjust
instance DeleteMap (Data.IntMap.Strict.IntMap v) where
  unsafeDelete = Data.IntMap.Strict.delete
  maybeDelete = Data.IntMap.Strict.delete
instance UpsertMap (Data.IntMap.Strict.IntMap v) where
  upsert = Data.IntMap.Strict.insert
instance UpleteMap (Data.IntMap.Strict.IntMap v) where
  adlete = Data.IntMap.Strict.update
instance AlterMap  (Data.IntMap.Strict.IntMap v) where
  alter = Data.IntMap.Strict.alter
  alterF_ = toCoyonedaTransform Data.IntMap.Strict.alterF

instance LookupMap (Lazy (Data.IntMap.Lazy.IntMap v)) where
  lookup = unwrapCoerce1 Data.IntMap.Lazy.lookup
  index = unwrapCoerce1 $ flip (Data.IntMap.Lazy.!)
  member = unwrapCoerce1 Data.IntMap.Lazy.member
  notMember = unwrapCoerce1 Data.IntMap.Lazy.notMember
instance SingletonMap (Lazy (Data.IntMap.Lazy.IntMap v)) where
  singleton k v = Lazy $ Data.IntMap.Lazy.singleton k v
instance InsertMap (Lazy (Data.IntMap.Lazy.IntMap v)) where
  unsafeInsert = rewrapCoerce2 Data.IntMap.Lazy.insert
instance UpdateMap (Lazy (Data.IntMap.Lazy.IntMap v)) where
  unsafeUpdate = rewrapCoerce2 Data.IntMap.Lazy.insert
  unsafeAdjust = rewrapCoerce2 Data.IntMap.Lazy.adjust
  maybeAdjust = rewrapCoerce2 Data.IntMap.Lazy.adjust
instance DeleteMap (Lazy (Data.IntMap.Lazy.IntMap v)) where
  unsafeDelete = rewrapCoerce1 Data.IntMap.Lazy.delete
  maybeDelete = rewrapCoerce1 Data.IntMap.Lazy.delete
instance UpsertMap (Lazy (Data.IntMap.Lazy.IntMap v)) where
  upsert = rewrapCoerce2 Data.IntMap.Lazy.insert
instance UpleteMap (Lazy (Data.IntMap.Lazy.IntMap v)) where
  adlete = rewrapCoerce2 Data.IntMap.Lazy.update
instance AlterMap  (Lazy (Data.IntMap.Lazy.IntMap v)) where
  alter = rewrapCoerce2 Data.IntMap.Lazy.alter
  alterF_ = toCoyonedaTransform (rewrapCoerce2F Data.IntMap.Lazy.alterF)


type instance Key (Set a) = a
type instance Value (Set a) = ()
{-
I've made 'Set's both strict and lazy. Why?

Well all maps are assumed to have strict keys.

Strict maps store strict values, and lazy maps store lazy values.

But what does this mean?

Strict maps will not store completely unevaluated thunks as values,
they will evaluate them to at least WHNF.

Lazy maps will not evaluate their value arguments at all.

What do sets do? Well sets have a fake value type, '()'. They essentially only store keys, not values.

Are they strict value wise? Well yes in a sense that they don't store unevaluated thunks.
Are they lazy value wise? Well yes as they don't evalute their value arguments (they don't really have any).

In the end this is largely academic I suspect anyway.
-}
instance IsStrictMap (Set a)
instance IsLazyMap (Set a)

instance Ord a => SingletonMap (Set a) where
  singleton k _ = Data.Set.singleton k
instance Ord a => LookupMap (Set a) where
  lookup k x = toMaybe (member k x) ()
  member = Data.Set.member
  index k x = if member k x then () else error "Class 'LookupMap', instance 'Set', function 'index': Index not found."
  unsafeIndex _ _ = ()
instance Ord a => InsertMap (Set a) where
  unsafeInsert k _ = Data.Set.insert k
  {-|
  Note that 'Data.Set.insert' may replace a key with an "equal" key
  i.e. on that is equal under '(==)' of the 'Eq' class.

  So technically this function may returned a modified set even if the key
  is already in the set.

  But I don't think this is an unreasonable violation of the specification.
  -}
  maybeInsert k _ = Data.Set.insert k
instance Ord a => DeleteMap (Set a) where
  unsafeDelete = Data.Set.delete
  maybeDelete = Data.Set.delete
instance Ord a => UpdateMap (Set a) where
  unsafeUpdate _ _ = id
  unsafeAdjust _ _ = id
instance Ord a => UpsertMap (Set a) where
  upsert k _ = Data.Set.insert k
instance Ord a => UpleteMap (Set a)
instance Ord a => AlterMap (Set a)

type instance Key IntSet = Int
type instance Value IntSet = ()

instance SingletonMap IntSet where
  singleton k _ = Data.IntSet.singleton k
instance LookupMap IntSet where
  lookup k x = toMaybe (member k x) ()
  member = Data.IntSet.member
  index k x = case member k x of
    True -> ()
    False -> error "Class 'LookupMap', instance 'IntSet', function 'index': Index not found."
  unsafeIndex _ _ = ()
instance InsertMap IntSet where
  unsafeInsert k _ = Data.IntSet.insert k
  maybeInsert k _ = Data.IntSet.insert k
instance DeleteMap IntSet where
  unsafeDelete = Data.IntSet.delete
  maybeDelete = Data.IntSet.delete
instance UpdateMap IntSet where
  unsafeUpdate _ _ x = x
  unsafeAdjust _ _ x = x
instance UpsertMap IntSet where
  upsert k _ = Data.IntSet.insert k
instance UpleteMap IntSet
instance AlterMap IntSet

type instance Key (Seq a) = Int
type instance Value (Seq a) = a
instance LookupMap (Seq a) where
  lookup = Control.Class.Impl.Map.CPP.seqLookup
  index = flip Data.Sequence.index
  member k x = 0 <= k && k < length x
instance UpdateMap (Seq a) where
  unsafeAdjust = Data.Sequence.adjust'
  maybeAdjust = Data.Sequence.adjust'
  unsafeUpdate = Data.Sequence.update
  maybeUpdate = Data.Sequence.update
  safeAdjustF_ = defaultSafeAdjustFBasedOnUnsafeUpdate
instance AppendMap (Seq a) where
  append v x = x Data.Sequence.|> v
  appendGetKey v x = (Data.Sequence.length x, append v x)

type instance Key (Array i e) = i
type instance Value (Array i e) = e
instance IsLazyMap (Array i e)

instance Ix i => LookupMap (Array i e) where
  index = flip (Data.Array.IArray.!)
  member k x = let (lbound, ubound) = Data.Array.IArray.bounds x in (lbound <= k && k <= ubound)
instance Ix i => SingletonMap (Array i e) where
  singleton k v = Data.Array.IArray.array (k,k) [(k,v)]

type instance Key Data.ByteString.ByteString = Int
type instance Value Data.ByteString.ByteString = Word8

instance LookupMap Data.ByteString.ByteString where
  index = flip Data.ByteString.index
  member k x = 0 <= k && k < Data.ByteString.length x
  unsafeIndex = flip Data.ByteString.Unsafe.unsafeIndex

type instance Key Data.ByteString.Lazy.ByteString = Int64
type instance Value Data.ByteString.Lazy.ByteString = Word8

instance LookupMap Data.ByteString.Lazy.ByteString where
  index = flip Data.ByteString.Lazy.index
  member k x = 0 <= k && k < Data.ByteString.Lazy.length x

type instance Key Data.ByteString.Short.ShortByteString = Int
type instance Value Data.ByteString.Short.ShortByteString = Word8

instance LookupMap Data.ByteString.Short.ShortByteString where
  index = flip Data.ByteString.Short.index
  member k x = 0 <= k && k < Data.ByteString.Short.length x

(!) :: LookupMap t => t -> Key t -> Value t
(!) = flip index
