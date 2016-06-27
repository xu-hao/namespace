{-# LANGUAGE RankNTypes, StandaloneDeriving, GADTs, FlexibleInstances #-}

module Data.Namespace.Namespace
    ( Namespace, lookupNamespace, lookupObject, topLevelObjects, insertObject,
      importFromNamespace, importAllFromNamespace, importExceptFromNamespace,
      importQualifiedFromNamespace, importQualifiedAllFromNamespace, importQualifiedExceptFromNamespace
    ) where

import Prelude hiding (lookup)
import Data.Map.Strict
import Data.Monoid
import Data.Maybe
import Control.Monad
import Control.Applicative

import Data.Namespace.Path

data Namespace k a where
  Namespace :: Key k => Map k (Namespace k a) -> Map k a -> Namespace k a

deriving instance (Show k, Show a) => Show (Namespace k a)

lookupNamespace :: Key k => NamespacePath k -> Namespace k a -> Maybe (Namespace k a)
lookupNamespace (NamespacePath []) n = return n
lookupNamespace (NamespacePath (k : ks)) (Namespace nm om) = do
  n' <- lookup k nm
  lookupNamespace (NamespacePath ks) n'

lookupObject :: Key k => ObjectPath k -> Namespace k a -> Maybe a
lookupObject (ObjectPath np k) n = do
  (Namespace nm om) <- lookupNamespace np n
  lookup k om

instance Key k => Monoid (Namespace k a) where
  mempty = Namespace mempty mempty
  mappend (Namespace nm om) (Namespace nm2 om2) = Namespace (unionWith mappend nm nm2) (om <> om2)

topLevelObjects :: Key k => Namespace k a -> Map k a
topLevelObjects (Namespace _ om) = om

insertObject :: Key k => ObjectPath k -> a -> Namespace k a -> Namespace k a
insertObject (ObjectPath (NamespacePath []) k) o (Namespace nm om) = Namespace nm (insert k o om)
insertObject (ObjectPath (NamespacePath (k : ks)) objkey) o (Namespace nm om) =
  let nm2 = fromMaybe mempty (lookup k nm)
      nm2' = insertObject (ObjectPath (NamespacePath ks) objkey) o nm2 in
      Namespace (insert k nm2' nm) om

insertNamespace :: Key k => NamespacePath k -> Map k a -> Namespace k a -> Namespace k a
insertNamespace (NamespacePath []) om (Namespace nm2 om2) = Namespace nm2 (om <> om2)
insertNamespace (NamespacePath (k : ks)) om (Namespace nm2 om2) =
  let nm3 = fromMaybe mempty (lookup k nm2)
      nm3' = insertNamespace (NamespacePath ks) om nm3' in
      Namespace (insert k nm3' nm2) om

importAllFromNamespace :: Key k => NamespacePath k -> Namespace k a -> Namespace k a -> Maybe (Namespace k a)
importAllFromNamespace np n (Namespace nm2 om2) = do
  n' <- lookupNamespace np n
  return (Namespace nm2 (topLevelObjects n' <> om2))

importFromNamespace :: Key k => NamespacePath k -> [k] -> Namespace k a -> Namespace k a -> Maybe (Namespace k a)
importFromNamespace np keys n (Namespace nm2 om2) = do
  n' <- lookupNamespace np n
  let om = topLevelObjects n'
  Namespace nm2 <$> foldM (\om2' key -> do
                                o <- lookup key om
                                return (insert key o om2')) om2 keys

importExceptFromNamespace :: Key k => NamespacePath k -> [k] -> Namespace k a -> Namespace k a -> Maybe (Namespace k a)
importExceptFromNamespace np keys n (Namespace nm2 om2) = do
  n' <- lookupNamespace np n
  let om = topLevelObjects n'
  return (Namespace nm2 (foldlWithKey (\om2' key o ->
                                if key `elem` keys
                                  then om2'
                                  else insert key o om2') om2 om))

importQualifiedAllFromNamespace :: Key k => NamespacePath k -> Namespace k a -> Namespace k a -> Maybe (Namespace k a)
importQualifiedAllFromNamespace p n n2 = do
  n' <- lookupNamespace p n
  return (insertNamespace p (topLevelObjects n') n2)

importQualifiedFromNamespace :: Key k => NamespacePath k -> [k] -> Namespace k a -> Namespace k a -> Maybe (Namespace k a)
importQualifiedFromNamespace p keys n n2 = do
  n' <- lookupNamespace p n
  let om = topLevelObjects n'
  om' <- foldM (\om2' key -> do
                            o <- lookup key om
                            return (insert key o om2')) mempty keys
  return (insertNamespace p om' n2)

importQualifiedExceptFromNamespace :: Key k => NamespacePath k -> [k] -> Namespace k a -> Namespace k a -> Maybe (Namespace k a)
importQualifiedExceptFromNamespace p keys n n2 = do
  n' <- lookupNamespace p n
  let om = topLevelObjects n'
  let om' = foldlWithKey (\om2' key o ->
                            if key `elem` keys
                              then om2'
                              else insert key o om2') mempty om
  return (insertNamespace p om' n2)
