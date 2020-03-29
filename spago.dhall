{ name = "my-project"
, dependencies = [ "console", "effect", "psci-support" ,"options"]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
