{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "run-keyboard"
, dependencies =
    [ "ansi", "console", "effect", "node-readline", "psci-support", "run" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
