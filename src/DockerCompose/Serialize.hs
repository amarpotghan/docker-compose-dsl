{-# LANGUAGE FlexibleInstances #-}

module DockerCompose.Serialize where

import           Data.Monoid
import           Data.Text.Lazy             as L
import           DockerCompose.ComposeTree
import           DockerCompose.SpecialChars

class Serializable a where
  serialize :: a -> L.Text


instance Serializable NodeName where
  serialize (NodeName name) = name

instance Serializable KeyRep where
  serialize (KeyRep key) = key

instance Serializable ValueRep where
  serialize (ValueRep value) = serialize Space <> value

instance Serializable SpecialChars where
  serialize = L.pack . show

instance (Serializable a) => Serializable [ a ] where
  serialize = foldMap serialize

instance Serializable PairedElem where
  serialize (PairedElem t v) = serialize t <> serialize Equals <> serialize v

instance Serializable Tag where
  serialize (Tag t) = t

instance Serializable Property where
  serialize (SingleVal k v) = serialize (Space <> Space) <>  serialize k <> serialize (Colon <> Space) <> serialize v <> serialize NewLine
  serialize (PlainList k vs) = serializeWithList k vs
  serialize (TaggedList k kvs) = serializeWithList k kvs

instance Serializable ContainerNode where
  serialize (ContainerNode name ps) = serialize name <> serialize (Colon <> NewLine) <> serialize ps

serializeWithList :: (Serializable k, Serializable v) => k -> [ v ] -> Text
serializeWithList k vs = let f = (<> serialize NewLine) . (serialize (Space <> Space <> Space <> Space <> Hyphen <> Space) <>) . serialize
                         in serialize (Space <> Space) <> serialize k <> serialize (Colon <> NewLine) <> foldMap f vs
