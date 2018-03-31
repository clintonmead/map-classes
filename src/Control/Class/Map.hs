{-|
Exports the functions non instances writers should need.

If you want to write your own instances (or indeed just want a general readme for the class)
see the module "Control.Class.Impl.Map"
-}
module Control.Class.Map (
  Key, Value,
  LookupMap(lookup, index, unsafeIndex, member, notMember),
  SingletonMap(singleton),
  InsertMap(insert, unsafeInsert, maybeInsert, safeInsert),
  UpdateMap(
    update, updateLookup, unsafeUpdate, unsafeUpdateLookup, maybeUpdate, safeUpdate, safeUpdateLookup,
    adjust, adjustLookup, unsafeAdjust, unsafeAdjustLookup, maybeAdjust, safeAdjust, safeAdjustLookup
    ), adjustF, unsafeAdjustF, safeAdjustF,
  DeleteMap(
    delete,    deleteLookup,    unsafeDelete,    unsafeDeleteLookup,    maybeDelete,    safeDelete,    safeDeleteLookup,
    optDelete, optDeleteLookup, unsafeOptDelete, unsafeOptDeleteLookup, maybeOptDelete, safeOptDelete, safeOptDeleteLookup
    ), optDeleteF, unsafeOptDeleteF, safeOptDeleteF,
  UpsertMap(
    upsert, upsertLookup
    ), adsertF,
  UpleteMap(
    adlete, adleteLookup, unsafeAdlete, unsafeAdleteLookup, maybeAdlete, safeAdlete, safeAdleteLookup
    ), adleteF, unsafeAdleteF, safeAdleteF,
  AlterMap(
    alter, alterLookup
    ), alterF,
  Strict(Strict), Lazy(Lazy),
  (!)
) where

import Control.Class.Impl.Map
