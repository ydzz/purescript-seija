{ name = "my-project"
, dependencies =
  [ "colors"
  , "console"
  , "effect"
  , "maybe"
  , "options"
  , "profunctor-lenses"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
