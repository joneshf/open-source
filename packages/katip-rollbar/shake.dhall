let Executable = ./../../Package/Haskell/Executable.dhall

let Manifest = ./../../Package/Haskell/Manifest.dhall

let Package = ./../../Package.dhall

let Test = ./../../Package/Haskell/Test.dhall

in    Package.Haskell
      { executables =
          [] : List Executable
      , manifest =
          Manifest.Hpack {=}
      , name =
          "katip-rollbar"
      , sourceDirectory =
          "src"
      , tests =
          [] : List Test
      , version =
          "0.3.0.0"
      }
    : Package
