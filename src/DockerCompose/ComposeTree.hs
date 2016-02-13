{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module DockerCompose.ComposeTree where

import           Control.Monad.State
import           Data.List           as L
import           Data.Monoid
import           Data.String
import           Data.Text.Lazy
import           GHC.TypeLits

newtype DockerCompose m a = DockerCompose { runDockerCompose :: StateT ComposeTree m a } deriving (Functor, Applicative, Monad, MonadState ComposeTree)

newtype NodeName = NodeName { getNodeName :: Text } deriving (Eq, Show, Read, Ord)

newtype KeyRep = KeyRep { keyRep :: Text } deriving (Show, Eq)
newtype ValueRep = ValueRep { valueRep :: Text } deriving (Show, Eq)

type ComposeTree = [ ContainerNode ]

data ContainerNode = ContainerNode { nodeName       :: NodeName
                                   , nodeProperties :: [ Property ]
                                   } deriving (Show, Eq)

single :: Text -> ContainerNode
single name = ContainerNode { nodeName = NodeName name
                            , nodeProperties = []
                            }


newtype Tag = Tag Text deriving (Show, Eq)

data PairedElem = PairedElem Tag ValueRep deriving (Show, Eq)

data Property = SingleVal KeyRep ValueRep
              | PlainList KeyRep [ ValueRep ]
              | TaggedList KeyRep [ PairedElem ] deriving (Show, Eq)

-- TODO: more efficently Traversable
appendProperty :: Property -> ComposeTree -> ComposeTree
appendProperty p = onLast addProp where
  addProp cn = cn { nodeProperties = (nodeProperties cn) ++ [ p ] }

appendValue :: ValueRep -> ComposeTree -> ComposeTree
appendValue v = onLast (\ cn -> cn { nodeProperties = onLast addVal (nodeProperties cn) } ) where
  addVal (PlainList k vs) = PlainList k (vs ++ [ v ])
  addVal x = x

onLast :: (a -> a) -> [ a ] -> [ a ]
onLast f = L.reverse . go . L.reverse where
  go (x : xs) = f x : xs
  go [] = []

execDockerCompose :: (Monad m) => ComposeTree -> DockerCompose m a -> m ComposeTree
execDockerCompose c = ($ c) . execStateT . runDockerCompose
