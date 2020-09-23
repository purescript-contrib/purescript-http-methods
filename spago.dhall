{ name = "http-methods"
, dependencies =
  [ "console", "effect", "either", "prelude", "psci-support", "strings" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
