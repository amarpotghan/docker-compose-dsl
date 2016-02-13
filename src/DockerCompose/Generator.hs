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
  type family Arg a :: k
  generate :: (MonadState ComposeTree m)
             => proxy a
             -> Arg a
             -> m ()

instance (HasGenerator a, HasGenerator b ) => HasGenerator ( a :&: b ) where

  type Arg (a :&: b) =  Arg a :&: Arg b

  generate _ ( a :&: b) = generate (Proxy :: Proxy a) a
                          *> generate (Proxy :: Proxy b) b

instance (HasGenerator rest, KnownSymbol current) => HasGenerator (current :~ rest) where

  type Arg (current :~ rest) = Arg rest

  generate _ restArg = modify addContainer
                       *> generate (Proxy :: Proxy rest) restArg
    where
      addContainer = (++ [single $ L.pack (symbolVal (Proxy :: Proxy current))])

instance (HasGenerator a, HasGenerator b) => HasGenerator (a :& b) where
  type Arg (a :& b) = Arg a :& Arg b
  generate _ ( a :& b) = generate (Proxy :: Proxy a) a
                         *> generate (Proxy :: Proxy b) b
  
instance (KnownSymbol a, Show value, ToStringy s) => HasGenerator (s := Capture a value) where

  type Arg (s := Capture a value) = value

  generate _ v = modify $ appendProperty $ SingleVal k value where
    k = KeyRep $ L.pack (toStringy (Proxy :: Proxy s))
    value = ValueRep $ L.pack (show v)
    
instance ( Show a, ToStringy s) => HasGenerator (s := Value a) where

  type Arg (s := Value a) = a

  generate _ v = modify $ appendProperty $ SingleVal k value where
    k = KeyRep $ L.pack (toStringy (Proxy :: Proxy s))
    value = ValueRep $ L.pack (show v)

instance (KnownSymbol a, ToStringy s) => HasGenerator (s := a) where

  type Arg (s := a) = ()

  generate _ _ = modify $ appendProperty (SingleVal k v) where
    k = KeyRep . L.pack $ toStringy (Proxy :: Proxy s)
    v = ValueRep . L.pack $ symbolVal (Proxy :: Proxy a)

instance ( Show a, ToStringy s) => HasGenerator (s := Values a) where

  type Arg (s := Values a) = [ a ]

  generate _ v = modify $ appendProperty $ PlainList k values where
    k = KeyRep $ L.pack (toStringy (Proxy :: Proxy s))
    values = ValueRep . L.pack . show <$> v

instance (KnownSymbol a, ToStringy s, HasGenerator rest) => HasGenerator (s := (a ': rest)) where
                                                                               
   type Arg (s := (a ': rest)) = Arg (a ': rest)

   generate _ v = (modify $ appendProperty $ PlainList k [])
                    *> generate (Proxy :: Proxy (a ': rest)) v
    where
      k = KeyRep $ L.pack $ toStringy (Proxy :: Proxy s)
  
     
instance (KnownSymbol a, HasGenerator rest) => HasGenerator (a ': rest) where

  type Arg (a ': rest) = Arg rest

  generate _ restHandler = do
    modify $ appendValue $ ValueRep (L.pack $ symbolVal (Proxy :: Proxy a))
    generate (Proxy :: Proxy rest) restHandler

instance HasGenerator '[] where

  type Arg '[] = ()

  generate _ _ = return ()
