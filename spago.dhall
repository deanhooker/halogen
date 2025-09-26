{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "css"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "halogen-subscriptions"
  , "maybe"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
