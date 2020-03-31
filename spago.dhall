{ name = "my-project"
, dependencies = [ "console", "effect", "psci-support" ,"options","colors","maybe"]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
