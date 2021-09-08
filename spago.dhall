{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "canvas"
  , "console"
  , "effect"
  , "halogen"
  , "psci-support"
  , "random"
  , "record"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
