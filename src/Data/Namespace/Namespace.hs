{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances #-}

module Data.Namespace.Namespace
    ( Namespace, lookupNamespace, lookupObject, namespace, topLevelObjects, insertObject
    ) where

import Prelude hiding (lookup)
import Data.Map.Strict
import Data.Monoid
import Data.Maybe

import Data.Namespace.Path

data Namespace k a where
  Namespace :: Key k => Map k (Namespace k a) -> Map k a -> Namespace k a

lookupNamespace :: Key k => NamespacePath k -> Namespace k a -> Maybe (Namespace k a)
lookupNamespace (NamespacePath []) n = return n
lookupNamespace (NamespacePath (k : ks)) (Namespace nm om) = do
  n' <- lookup k nm
  lookupNamespace (NamespacePath ks) n'

lookupObject :: Key k => ObjectPath k -> Namespace k a -> Maybe a
lookupObject (ObjectPath np k) n = do
  (Namespace nm om) <- lookupNamespace np n
  lookup k om

namespace :: Key k => Namespace k a
namespace = Namespace mempty mempty

topLevelObjects :: Key k => Namespace k a -> Map k a
topLevelObjects (Namespace _ om) = om

insertObject :: Key k => ObjectPath k -> a -> Namespace k a -> Namespace k a
insertObject (ObjectPath (NamespacePath []) k) o (Namespace nm om) = Namespace nm (insert k o om)
insertObject (ObjectPath (NamespacePath (k : ks)) objkey) o (Namespace nm om) =
  let nm2 = fromMaybe namespace (lookup k nm)
      nm2' = insertObject (ObjectPath (NamespacePath ks) objkey) o nm2 in
      Namespace (insert k nm2' nm) om
