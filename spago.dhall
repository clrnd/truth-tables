{ name =
    "truth-tables"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "generics-rep"
    , "parsing"
    , "prelude"
    , "psci-support"
    , "react-basic"
    , "react-basic-hooks"
    , "spec"
    , "strings"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
