{ name = "my-project"
, dependencies =
  [ "colors"
  , "console"
  , "effect"
  , "maybe"
  , "options"
  , "sized-vectors"
  , "profunctor-lenses"
  , "psci-support"
  , "argonaut-core"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
