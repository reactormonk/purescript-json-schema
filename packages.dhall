{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.3-20210808/packages.dhall sha256:dbc06803c031113d3f9e001f8d95629e48da720d2bfe45d8bbe2c0cffcef293d

in  upstream
  with dodo-printer =
    { dependencies =
      [ "aff"
      , "ansi"
      , "avar"
      , "console"
      , "effect"
      , "foldable-traversable"
      , "lists"
      , "maybe"
      , "minibench"
      , "node-child-process"
      , "node-fs-aff"
      , "node-process"
      , "psci-support"
      , "strings"
      ]
    , repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
    , version = "v2.0.0"
    }
  with ps-cst =
    { dependencies =
      [ "console"
      , "effect"
      , "psci-support"
      , "record"
      , "strings"
      , "spec"
      , "node-path"
      , "node-fs-aff"
      , "ansi"
      , "dodo-printer"
      ]
    , repo = "https://github.com/purescript-codegen/purescript-ps-cst.git"
    , version = "1339dd3"
    }
  with cst-simple =
    { dependencies =
      [ "arrays"
      , "console"
      , "debug"
      , "effect"
      , "node-fs-aff"
      , "ps-cst"
      , "psci-support"
      , "spec"
      , "string-parsers"
      , "typelevel-prelude"
      , "unicode"
      , "prelude"
      ]
    , repo = "https://github.com/purescript-codegen/purescript-cst-simple.git"
    , version = "37d49e8"
    }
