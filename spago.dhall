{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen"
, dependencies =
  [ "aff"
  , "console"
  , "const"
  , "css"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "maybe"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
