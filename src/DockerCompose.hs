module DockerCompose
    (
      module DockerCompose.API
    , module DockerCompose.Generator
    , module DockerCompose.Serialize
    , module DockerCompose.SpecialChars
    , module DockerCompose.ComposeTree
    , module DockerCompose.Keys
    , toFile
    , asText
    , composeTree
    ) where

import           Control.Monad.Identity
import           Control.Monad.Writer       (execWriter)
import           Data.Proxy
import           Data.Text.Lazy             as L
import           DockerCompose.API
import           DockerCompose.ComposeTree
import           DockerCompose.Generator
import           DockerCompose.Keys
import           DockerCompose.Serialize
import           DockerCompose.SpecialChars
import           System.IO

toFile :: (HasGenerator a) => FilePath -> Proxy a -> Arg a -> IO ()
toFile file p = (writeFile file . L.unpack . serialize =<<) . execDockerCompose mempty . generate p

asText :: (HasGenerator a) => Proxy a -> Arg a -> Text
asText p = serialize . runIdentity . execDockerCompose mempty . generate p

composeTree :: (HasGenerator a) => Proxy a -> Arg a -> ComposeTree
composeTree p = runIdentity . execDockerCompose mempty . generate p


