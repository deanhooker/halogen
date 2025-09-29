{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "const"
  , "control"
  , "css"
  , "datetime"
  , "effect"
  , "exceptions"
  , "halogen"
  , "halogen-css"
  , "halogen-subscriptions"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "tailrec"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
