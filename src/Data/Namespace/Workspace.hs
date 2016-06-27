{-# LANGUAGE RankNTypes, GADTs #-}

module Data.Namespace.Workspace
    ( Workspace, workspace, lookupNamespace, lookupObject, importFromNamespace, importAllFromNamespace, importExceptFromNamespace, importQualifiedFromNamespace, importQualifiedAllFromNamespace, importQualifiedExceptFromNamespace, lookupFromWorkspace
    ) where

import Prelude hiding (lookup)
import Data.Map.Strict
import Data.Monoid
import Control.Monad
import Control.Applicative

import Data.Namespace.Namespace
import Data.Namespace.Path

data Workspace k a where
  Workspace :: Key k => Map (NamespacePath k) (Map k a) -> Map k a -> Workspace k a

workspace :: Key k => Workspace k a
workspace = Workspace mempty mempty

importAllFromNamespace :: Key k => Namespace k a -> Workspace k a -> Workspace k a
importAllFromNamespace n (Workspace nm2 om2) = Workspace nm2 (topLevelObjects n <> om2)

importFromNamespace :: Key k => [k] -> Namespace k a -> Workspace k a -> Maybe (Workspace k a)
importFromNamespace keys n (Workspace nm2 om2) =
  let om = topLevelObjects n in
      Workspace nm2 <$> foldM (\om2' key -> do
                                o <- lookup key om
                                return (insert key o om2')) om2 keys

importExceptFromNamespace :: Key k => [k] -> Namespace k a -> Workspace k a -> Workspace k a
importExceptFromNamespace keys n (Workspace nm2 om2) =
  let om = topLevelObjects n in
      Workspace nm2 (foldlWithKey (\om2' key o ->
                                if key `elem` keys
                                  then om2'
                                  else insert key o om2') om2 om)

importQualifiedAllFromNamespace :: Key k => NamespacePath k -> Namespace k a -> Workspace k a -> Maybe (Workspace k a)
importQualifiedAllFromNamespace p n (Workspace nm2 om2) = do
  n' <- lookupNamespace p n
  return (Workspace (insert p (topLevelObjects n') nm2) om2)

importQualifiedFromNamespace :: Key k => NamespacePath k -> [k] -> Namespace k a -> Workspace k a -> Maybe (Workspace k a)
importQualifiedFromNamespace p keys n (Workspace nm2 om2) = do
  n' <- lookupNamespace p n
  let om = topLevelObjects n'
  om' <- foldM (\om2' key -> do
                            o <- lookup key om
                            return (insert key o om2')) mempty keys
  return (Workspace (insert p om' nm2) om2)

importQualifiedExceptFromNamespace :: Key k => NamespacePath k -> [k] -> Namespace k a -> Workspace k a -> Maybe (Workspace k a)
importQualifiedExceptFromNamespace p keys n (Workspace nm2 om2) = do
  n' <- lookupNamespace p n
  let om = topLevelObjects n'
  let om' = foldlWithKey (\om2' key o ->
                            if key `elem` keys
                              then om2'
                              else insert key o om2') mempty om
  return (Workspace (insert p om' nm2) om2)

lookupFromWorkspace :: Key k => ObjectPath k -> Workspace k a -> Maybe a
lookupFromWorkspace op@(ObjectPath np k) (Workspace nm om) =
  if qualified op
    then do
      om2 <- lookup np nm
      lookup k om2
    else
      lookup k om
