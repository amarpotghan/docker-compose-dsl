module DockerCompose
    (
      module DockerCompose.API
    , module DockerCompose.Generator
    , toFile
    , asString
    ) where

import           Control.Monad.Writer    (runWriter)
import           Data.Proxy
import           DockerCompose.API
import           DockerCompose.Generator
import           System.IO

toFile :: (HasGenerator a) => FilePath -> Proxy a -> Handler a -> IO ()
toFile file p = writeFile file . snd . runWriter . generate p

asString :: (HasGenerator a) => Proxy a -> Handler a -> String
asString p = snd . runWriter . generate p
