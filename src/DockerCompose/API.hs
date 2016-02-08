{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeOperators     #-}
module DockerCompose.API where

import           Data.Proxy
import           GHC.TypeLits


{-
   >>> type First =  "app" :~
                         Image := String
                      :& Name  := String

   >>> type Second = "mail" :~
                          Image := String
                       :& Name  := "mail"
   >>> type MyComposeFile = First :&: Second

 -}
data (left :: *) :&: (right :: *) = left :&: right deriving (Eq, Show, Functor, Traversable, Foldable)
infixr 6 :&:


data (container :: Symbol) :~ (properties :: *)
infixr 7 :~

data (first :: *) :& (second :: *) = first :& second deriving (Eq, Show, Functor, Traversable, Foldable)
infixr 8 :&

data (key :: *) := (value :: *)
infixr 9 :=

data Value (value :: *)

data Values (values :: *)

data Capture (description :: Symbol) (value :: *)




