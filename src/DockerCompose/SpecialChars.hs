module DockerCompose.SpecialChars where


data SpecialChars = Space
                  | Colon
                  | Hyphen
                  | NewLine
                  | Equals
                  | BlankChar
                  | SpecialChars :. SpecialChars
                    deriving (Eq)

instance Show SpecialChars where
  show Space = " "
  show Colon = ":"
  show Hyphen = "-"
  show NewLine = "\n"
  show Equals = "="
  show BlankChar = mempty
  show (x :. y) = show $ x `mappend` y


instance Monoid SpecialChars where
  mempty = BlankChar
  x `mappend` y = x :. y
