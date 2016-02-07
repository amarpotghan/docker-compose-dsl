module DockerCompose
    (
      module DockerCompose.API
    , module DockerCompose.Generator
    , toFile
    , asString
    ) where

import           Control.Monad.Identity
import           Control.Monad.Writer      (execWriter)
import           Data.Proxy
import           DockerCompose.API
import           DockerCompose.ComposeTree
import           DockerCompose.Generator
import           System.IO

toFile :: (HasGenerator a) => Proxy a -> Handler a -> IO ComposeTree
toFile p = execDockerCompose mempty . generate p

asString :: (HasGenerator a) => Proxy a -> Handler a -> ComposeTree
asString p = runIdentity . execDockerCompose mempty . generate p


