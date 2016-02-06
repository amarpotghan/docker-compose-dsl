{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module DockerCompose.Generator where

import           Control.Monad.Writer
import           Data.Monoid
import DockerCompose.API
import Data.Proxy
import GHC.TypeLits
import DockerCompose.Keys

newLine :: String
newLine = "\n"

colon :: String
colon = ":"

space :: String
space = " "

hyphen :: String
hyphen = "-"

class HasGenerator a where
  type family Handler a :: k
  generate :: (MonadWriter String m) => Proxy a -> Handler a -> m ()

instance (HasGenerator a, HasGenerator b) => HasGenerator ( a :&: b ) where
  type Handler (a :&: b) =  Handler a :&: Handler b
  generate _ ( a :&: b) = generate (Proxy :: Proxy a) a *> generate (Proxy :: Proxy b) b

instance (HasGenerator rest, KnownSymbol current) => HasGenerator (current :~ rest) where
  type Handler (current :~ rest) = Handler rest
  generate _ restHandler = do
    tell $ symbolVal (Proxy :: Proxy current) <> colon <> newLine 
    generate (Proxy :: Proxy rest) restHandler

instance (HasGenerator a, HasGenerator b) => HasGenerator (a :& b) where
  type Handler (a :& b) = Handler a :& Handler b
  generate _ ( a :& b) = generate (Proxy :: Proxy a) a *> generate (Proxy :: Proxy b) b
  
instance (KnownSymbol a, Show value, ToStringy s) => HasGenerator (s := Capture a value) where
  type Handler (s := Capture a value) = value
  generate _ v = tell $ space <> space <> toStringy (Proxy :: Proxy s) <> space <> (show v) <> newLine
    

instance (Show a, ToStringy s) => HasGenerator (s := Value a) where
  type Handler (s := Value a) = a
  generate _ v = tell $ space <> space <> toStringy (Proxy :: Proxy s) <> colon <> space <> (show v) <> newLine

instance (Show a, ToStringy s) => HasGenerator (s := Values a) where
  type Handler (s := Values a) = [ a ]
  generate _ v = let spit a = space <> space <> space <> space <> hyphen <> show a <> newLine
                 in tell $ space <> space <> toStringy (Proxy :: Proxy s) <> colon <> space <> foldMap spit v
