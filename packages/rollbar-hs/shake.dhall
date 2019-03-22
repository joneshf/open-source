let Executable = ./../../Package/Haskell/Executable.dhall

let Manifest = ./../../Package/Haskell/Manifest.dhall

let Package = ./../../Package.dhall

in    Package.Haskell
      { executables =
          [] : List Executable
      , manifest =
          Manifest.Hpack {=}
      , name =
          "rollbar-hs"
      , sourceDirectory =
          "src"
      , tests =
          [ { suite = "doc-test", testDirectory = "test" } ]
      , version =
          "0.3.1.0"
      }
    : Package
