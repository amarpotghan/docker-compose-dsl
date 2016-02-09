{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Data.Proxy
import           DockerCompose

main :: IO ()
main = toFile "docker-compose.yml" myDC ("mycompany/app" :& [ "mail" ] :&: "mycompany/mail" :& [ "someexternal" ] :&: () :& ())

myDC :: Proxy DSL
myDC = Proxy

newtype ContainerName = C String deriving Show

type Third = "newOne" :~
                Image := "capitalmatch/newImage"
             :& VolumesFrom := '[ "mail", "app" ]

type DSL = "app" :~
              Image := Value String :&
              VolumesFrom := Values String
          :&:
            "mail" :~
              Image := Value String :&
              VolumesFrom := Values String

          :&: Third
