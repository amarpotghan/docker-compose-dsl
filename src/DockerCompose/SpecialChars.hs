module DockerCompose.SpecialChars where


data SpecialChar = Space
                  | Colon
                  | Hyphen
                  | NewLine
                  | Equals
                  | BlankChar
                  | SpecialChar :. SpecialChar
                    deriving (Eq, Show)

toString :: SpecialChar -> String
toString Space = " "
toString Colon = ":"
toString Hyphen = "-"
toString NewLine = "\n"
toString Equals = "="
toString BlankChar = mempty
toString (x :. y) = show $ x `mappend` y


instance Monoid SpecialChar where
  mempty = BlankChar
  x `mappend` y = x :. y
