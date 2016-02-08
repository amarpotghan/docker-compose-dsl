{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module DockerCompose.Generator where

import           Control.Monad.State
import           Data.Monoid
import DockerCompose.API
import Data.Proxy
import GHC.TypeLits
import DockerCompose.Keys
import DockerCompose.ComposeTree
import Data.String
import Data.Text.Lazy as L


class HasGenerator a where
  type family Handler a :: k
  generate :: (MonadState ComposeTree m) => Proxy a -> Handler a -> m ()

instance (HasGenerator a, HasGenerator b) => HasGenerator ( a :&: b ) where
  type Handler (a :&: b) =  Handler a :&: Handler b
  generate _ ( a :&: b) = generate (Proxy :: Proxy a) a *> generate (Proxy :: Proxy b) b

instance (HasGenerator rest, KnownSymbol current) => HasGenerator (current :~ rest) where
  type Handler (current :~ rest) = Handler rest
  generate _ restHandler = modify (++ [(single . L.pack . symbolVal $ (Proxy :: Proxy current))]) *> generate (Proxy :: Proxy rest) restHandler

instance (HasGenerator a, HasGenerator b) => HasGenerator (a :& b) where
  type Handler (a :& b) = Handler a :& Handler b
  generate _ ( a :& b) = generate (Proxy :: Proxy a) a *> generate (Proxy :: Proxy b) b
  
instance (KnownSymbol a, Show value, ToStringy s) => HasGenerator (s := Capture a value) where
  type Handler (s := Capture a value) = value
  generate _ v = modify $ appendProperty $ SingleVal (KeyRep . L.pack $ toStringy (Proxy :: Proxy s))  (ValueRep . L.pack . show $ v) 
    
instance (Show a, ToStringy s) => HasGenerator (s := Value a) where
  type Handler (s := Value a) = a
  generate _ v = modify $ appendProperty $ SingleVal (KeyRep . L.pack $ toStringy (Proxy :: Proxy s))  (ValueRep . L.pack . show $ v) 

instance (Show a, ToStringy s) => HasGenerator (s := Values a) where
  type Handler (s := Values a) = [ a ]
  generate _ v = modify $ appendProperty $ PlainList (KeyRep $ L.pack $ toStringy (Proxy :: Proxy s)) (ValueRep . L.pack . show <$> v)
