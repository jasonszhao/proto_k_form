{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "bifunctors"
    , "console"
    , "control"
    , "effect"
    , "exceptions"
    , "foldable-traversable"
    , "halogen"
    , "integers"
    , "lazy"
    , "lists"
    , "math"
    , "numbers"
    , "ordered-collections"
    , "psci-support"
    , "quickcheck"
    , "react-dom"
    , "spec"
    , "spec-quickcheck"
    , "string-parsers"
    , "thermite"
    , "thermite-dom"
    , "transformers"
    , "tuples"
    , "unicode"
    , "web-html"
    , "yarn"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
