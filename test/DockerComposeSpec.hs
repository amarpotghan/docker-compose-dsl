{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module DockerComposeSpec where

import           Data.Proxy
import           DockerCompose
import           Test.Hspec

type NameBinder = "app" :~
                    Image := "something"

type PlainListTest = "app" :~
                       VolumesFrom := ["cont1", "cont2", "cont3"]

type SingleArg = "app" :~
                   Image := Value String

type LeftGroup = ("app" :~ Image := "somthing" :&: "mail" :~ Image := "else") :&: "bank" :~ Image := "other"
type RightGroup = "app" :~ Image := "somthing" :&: ("mail" :~ Image := "else" :&: "bank" :~ Image := "other")

type PropertyLeftGroup = "app" :~ (Image := "somthing" :& Environment := "env") :& VolumesFrom := "cont1"

type PropertyRightGroup = "app" :~ Image := "somthing" :& (Environment := "env" :& VolumesFrom := "cont1")

dockerComposeSpec :: Spec
dockerComposeSpec = describe "Specifications" $ do

  describe "Generator specs" $ do
    let createTree p =  execDockerCompose mempty . generate p

    it "on (:~) should add new node to the tree" $ do
      composeTree <- createTree (Proxy :: Proxy NameBinder) ()
      head composeTree `shouldBe` ContainerNode (NodeName "app") [SingleVal (KeyRep "image") (ValueRep "something")]

    it "on a type level list of strings should add PlainList to the tree" $ do
      composeTree <- createTree (Proxy :: Proxy PlainListTest) ()
      (nodeProperties . head $ composeTree) `shouldBe` [ PlainList (KeyRep "volumes_from") $ ValueRep <$> ["cont1", "cont2", "cont3"] ]

    it "accepts arguments of specified type" $ do
      composeTree <- createTree (Proxy :: Proxy SingleArg) "TestArg"
      (nodeProperties . head $ composeTree) `shouldBe` [ SingleVal (KeyRep "image") (ValueRep "\"TestArg\"")]

    it ":&: is associative" $ do
      composeTree1 <- createTree (Proxy :: Proxy LeftGroup) ((() :&: ()) :&: ())
      composeTree2 <- createTree (Proxy :: Proxy RightGroup) (() :&: (() :&: ()))

      composeTree1 `shouldBe` composeTree2

    it ":& should be associative" $ do
      composeTree1 <- createTree (Proxy :: Proxy PropertyLeftGroup) ((() :& ()) :& ())
      composeTree2 <- createTree (Proxy :: Proxy PropertyRightGroup) (() :& (() :& ()))

      composeTree1 `shouldBe` composeTree2







