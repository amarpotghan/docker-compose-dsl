module DockerCompose.Keys where

import           Data.Proxy

data Image
data VolumesFrom
data Environment
data Name

class ToStringy a where
  toStringy :: Proxy a -> String

instance ToStringy Image where
  toStringy _ = "image"

instance ToStringy VolumesFrom where
  toStringy _ = "volumes_from"

instance ToStringy Environment where
  toStringy _ = "environment"

instance ToStringy Name where
  toStringy _ = "container_name"
