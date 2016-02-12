module Main where

import           DockerCompose
import           DockerComposeSpec
import           Test.Hspec
main :: IO ()
main = mapM_ hspec [ dockerComposeSpec ]

