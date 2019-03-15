let Manifest = ./../../Manifest.dhall

let Package = ./../../Package.dhall

let Test = ./../../Test.dhall

in    { executables =
          [ { executableDirectory = "src", executableName = "build" } ]
      , manifest =
          Manifest.Cabal {=}
      , name =
          "build"
      , sourceDirectory =
          "src"
      , tests =
          [] : List Test
      , version =
          "0.1.0.0"
      }
    : Package
