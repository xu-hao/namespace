{-# LANGUAGE RankNTypes, GADTs, StandaloneDeriving, FlexibleInstances, PatternSynonyms #-}

module Data.Namespace.Path
    ( NamespacePath(..), ObjectPath(..), extendNamespacePath, concatNamespacePathWithObjectPath, qualified, Key,
    ) where

import Prelude hiding (lookup)
import Data.Map.Strict
import Data.Monoid

class Ord k => Key k where

data NamespacePath k where
  NamespacePath :: Key k => [k] -> NamespacePath k

deriving instance Eq (NamespacePath k)
deriving instance Ord (NamespacePath k)

instance Key (NamespacePath k) where

data ObjectPath k where
  ObjectPath :: Key k => NamespacePath k -> k -> ObjectPath k

extendNamespacePath :: Key k => NamespacePath k -> k -> NamespacePath k
extendNamespacePath (NamespacePath p) k = NamespacePath (p <> [k])

instance Key k => Monoid (NamespacePath k) where
  mempty = NamespacePath mempty
  mappend (NamespacePath p1) (NamespacePath p2) = NamespacePath (p1 <> p2)

concatNamespacePathWithObjectPath :: Key k => NamespacePath k -> ObjectPath k -> ObjectPath k
concatNamespacePathWithObjectPath np (ObjectPath np2 k) = ObjectPath (np <> np2) k

qualified :: Key k => ObjectPath k -> Bool
qualified (ObjectPath (NamespacePath []) _) = False
qualified _ = True
