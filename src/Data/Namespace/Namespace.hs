{-# LANGUAGE RankNTypes, StandaloneDeriving, GADTs, FlexibleInstances #-}

module Data.Namespace.Namespace
    ( Namespace, lookupNamespace, lookupObject, topLevelObjects, insertObject, insertNamespace, topLevelNamespaces, allObjects,
      importFromNamespace, importAllFromNamespace, importExceptFromNamespace,
      importQualifiedFromNamespace, importQualifiedAllFromNamespace, importQualifiedExceptFromNamespace,
      importFromNamespaceE, importAllFromNamespaceE, importExceptFromNamespaceE,
      importQualifiedFromNamespaceE, importQualifiedAllFromNamespaceE, importQualifiedExceptFromNamespaceE, toTree
    ) where

import Prelude hiding (lookup)
import Data.Map.Strict (lookup, insert, foldlWithKey, mapKeys, mapWithKey, unionWith, toList, Map, elems)
import Data.Monoid
import Data.Monoid.Action (act)
import Data.Maybe
import Data.Tree
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

topLevelNamespaces :: Key k => Namespace k a -> Map k (Namespace k a)
topLevelNamespaces (Namespace nm _) = nm

concatKey :: Key k => k -> Map (ObjectPath k) a -> Map (ObjectPath k) a
concatKey key = mapKeys (act (NamespacePath [key]))

pathKey :: Key k => Map k a -> Map (ObjectPath k) a
pathKey = mapKeys (ObjectPath (NamespacePath []))

allObjects :: Key k => Namespace k a -> Map (ObjectPath k) a
allObjects ns = pathKey (topLevelObjects ns) <> mconcat (elems (mapWithKey (\key ns2 -> concatKey key (allObjects ns2)) (topLevelNamespaces ns)))

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
      nm3' = insertNamespace (NamespacePath ks) om nm3 in
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

importAllFromNamespaceE :: (Key k, Show k, Show a) => NamespacePath k -> Namespace k a -> Namespace k a -> Either String (Namespace k a)
importAllFromNamespaceE np n (Namespace nm2 om2) =
  case lookupNamespace np n of
    Nothing -> Left ("importAllFromNamespaceE: cannot find " ++ show np ++ " in " ++ drawTree (fmap show (toTree n)))
    Just n' -> do
      return (Namespace nm2 (topLevelObjects n' <> om2))

importFromNamespaceE :: (Key k, Show k, Show a) => NamespacePath k -> [k] -> Namespace k a -> Namespace k a -> Either String (Namespace k a)
importFromNamespaceE np keys n (Namespace nm2 om2) =
  case lookupNamespace np n of
    Nothing -> Left ("importFromNamespaceE: cannot find " ++ show np ++ " in " ++ drawTree (fmap show (toTree n)))
    Just n' -> do
      let om = topLevelObjects n'
      Namespace nm2 <$> foldM (\om2' key ->
                                case lookup key om of
                                  Nothing ->
                                    Left ("importFromNamespaceE: cannot find " ++ show key ++ " at " ++ show np ++ " in " ++ show om)
                                  Just o ->
                                    return (insert key o om2')) om2 keys

importExceptFromNamespaceE :: (Key k, Show k, Show a) => NamespacePath k -> [k] -> Namespace k a -> Namespace k a -> Either String (Namespace k a)
importExceptFromNamespaceE np keys n (Namespace nm2 om2) =
  case lookupNamespace np n of
    Nothing -> Left ("importExceptFromNamespaceE: cannot find " ++ show np ++ " in " ++ drawTree (fmap show (toTree n)))
    Just n' -> do
      let om = topLevelObjects n'
      return (Namespace nm2 (foldlWithKey (\om2' key o ->
                                    if key `elem` keys
                                      then om2'
                                      else insert key o om2') om2 om))

importQualifiedAllFromNamespaceE :: (Key k, Show k, Show a) => NamespacePath k -> Namespace k a -> Namespace k a -> Either String (Namespace k a)
importQualifiedAllFromNamespaceE p n n2 =
  case lookupNamespace p n of
    Nothing -> Left ("importQualifiedFromNamespaceE: cannot find " ++ show p ++ " in " ++ drawTree (fmap show (toTree n)))
    Just n' ->
      return (insertNamespace p (topLevelObjects n') n2)

importQualifiedFromNamespaceE :: (Key k, Show k, Show a) => NamespacePath k -> [k] -> Namespace k a -> Namespace k a -> Either String (Namespace k a)
importQualifiedFromNamespaceE p keys n n2 =
  case lookupNamespace p n of
    Nothing -> Left ("importQualifiedFromNamespaceE: cannot find " ++ show p ++ " in " ++ drawTree (fmap show (toTree n)))
    Just n' -> do
      let om = topLevelObjects n'
      om' <- foldM (\om2' key ->
                            case lookup key om of
                              Nothing ->
                                Left ("importQualifiedFromNamespaceE: cannot find " ++ show key ++ " at " ++ show p ++ " in " ++ show om)
                              Just o ->
                                return (insert key o om2')) mempty keys
      return (insertNamespace p om' n2)

importQualifiedExceptFromNamespaceE :: (Key k, Show k, Show a) => NamespacePath k -> [k] -> Namespace k a -> Namespace k a -> Either String (Namespace k a)
importQualifiedExceptFromNamespaceE p keys n n2 =
  case lookupNamespace p n of
    Nothing -> Left ("importQualifiedExceptFromNamespaceE: cannot find " ++ show p ++ " in " ++ drawTree (fmap show (toTree n)))
    Just n' -> do
      let om = topLevelObjects n'
      let om' = foldlWithKey (\om2' key o ->
                                if key `elem` keys
                                  then om2'
                                  else insert key o om2') mempty om
      return (insertNamespace p om' n2)

toTree :: Key k => Namespace k a -> Tree (Maybe k, Maybe a)
toTree ns =
  toTree' Nothing ns where
    toTree' rk (Namespace nm om) =
      Node (rk, Nothing) (map (\(k ,ns') -> toTree' (Just k) ns') (toList nm) ++ map (\(k, a) -> Node (Just k, Just a) []) (toList om))
