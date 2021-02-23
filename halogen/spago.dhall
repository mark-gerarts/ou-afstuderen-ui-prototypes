{ name = "halogen-prototype"
, dependencies = [ "affjax", "console", "effect", "halogen", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
