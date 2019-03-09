let Executable = ./../../Executable.dhall

let Manifest = ./../../Manifest.dhall

let Package = ./../../Package.dhall

let Test = ./../../Test.dhall

in    { executables =
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
