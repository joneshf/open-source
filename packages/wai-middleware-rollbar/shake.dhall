let Executable = ./../../Executable.dhall

let Manifest = ./../../Manifest.dhall

let Package = ./../../Package.dhall

let Test = ./../../Test.dhall

in    { executables =
          [] : List Executable
      , manifest =
          Manifest.Cabal {=}
      , name =
          "wai-middleware-rollbar"
      , sourceDirectory =
          "src"
      , tests =
          [] : List Test
      , version =
          "0.11.0"
      }
    : Package
