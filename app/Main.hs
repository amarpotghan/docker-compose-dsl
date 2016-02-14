{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Data.Proxy
import           DockerCompose

main :: IO ()
main = toFile "docker-compose.yml" myDSL dslArgs

type First = "app" :~
              Image := Value String :&
              VolumesFrom := Values String

type Second = "mail" :~
              Image := Value String :&
              VolumesFrom := Values String

type Third = "newOne" :~
                Image := "capitalmatch/newImage"
             :& VolumesFrom := '[ "mail", "app" ]

type DSL = First :&: Second :&: Third

myDSL :: Proxy DSL
myDSL = Proxy

dslArgs = containerFirstArgs :&: containerSecondArgs :&: containerThirdArgs
containerFirstArgs = "mycompany/app" :& [ "mail" ]
containerSecondArgs = "mycompany/mail" :& [ "someexternal" ]
containerThirdArgs = NoArg :& NoArg -- (should) no arguments needed!


