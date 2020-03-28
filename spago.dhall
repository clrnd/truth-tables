{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
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
