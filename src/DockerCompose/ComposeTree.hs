{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
module DockerCompose.ComposeTree where

import           Control.Monad.State
import           Data.List           as L
import           Data.Monoid
import           Data.String
import           Data.Text.Lazy
import           GHC.TypeLits

newtype NodeName = NodeName { getNodeName :: Text } deriving (Eq, Show, Read, Ord)

newtype KeyRep = KeyRep { keyRep :: Text }
newtype ValueRep = ValueRep { valueRep :: Text }

type ComposeTree = [ ContainerNode ]

data ContainerNode = ContainerNode { nodeName       :: NodeName
                                   , nodeProperties :: [ Property ]
                                   }

single :: Text -> ContainerNode
single name = ContainerNode { nodeName = NodeName name
                            , nodeProperties = []
                            }


newtype Tag = Tag Text

data PairedElem = PairedElem Tag ValueRep

data Property = SingleVal KeyRep ValueRep
              | PlainList KeyRep [ ValueRep ]
              | TaggedList KeyRep [ PairedElem ]

appendProperty :: Property -> ComposeTree -> ComposeTree
appendProperty p = L.reverse . go . L.reverse where
  go (x : xs) = x { nodeProperties = (nodeProperties x) <> [ p ] } : xs
  go [] = []

newtype DockerCompose m a = DockerCompose { runDockerCompose :: StateT ComposeTree m a } deriving (Functor, Applicative, Monad, MonadState ComposeTree)

execDockerCompose :: (Monad m) => ComposeTree -> DockerCompose m a -> m ComposeTree
execDockerCompose c = ($ c) . execStateT . runDockerCompose
