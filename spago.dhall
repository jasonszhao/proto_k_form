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
    , "foreign"
    , "foreign-generic"
    , "halogen"
    , "integers"
    , "lazy"
    , "lists"
    , "math"
    , "newtype"
    , "numbers"
    , "ordered-collections"
    , "psci-support"
    , "quickcheck"
    , "react-dom"
    , "read"
    , "spec"
    , "spec-quickcheck"
    , "storable"
    , "string-parsers"
    , "thermite"
    , "thermite-dom"
    , "transformers"
    , "tuples"
    , "unicode"
    , "web-html"
    , "web-storage"
    , "yarn"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
