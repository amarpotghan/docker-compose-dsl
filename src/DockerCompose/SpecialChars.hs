{-# LANGUAGE OverloadedStrings #-}
module DockerCompose.SpecialChars where

import           Data.Text.Lazy as L

data SpecialChar = Space
                  | Colon
                  | Hyphen
                  | NewLine
                  | Equals
                  | BlankChar
                  | SpecialChar :. SpecialChar
                    deriving (Eq, Show)

toText :: SpecialChar -> L.Text
toText Space = " "
toText Colon = ":"
toText Hyphen = "-"
toText NewLine = "\n"
toText Equals = "="
toText BlankChar = mempty
toText (x :. y) = toText $ x `mappend` y


instance Monoid SpecialChar where
  mempty = BlankChar
  x `mappend` y = x :. y
