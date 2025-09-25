{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen"
, dependencies =
  [ "console"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
