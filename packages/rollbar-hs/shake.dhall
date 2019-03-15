let Executable = ./../../Executable.dhall

let Manifest = ./../../Manifest.dhall

let Package = ./../../Package.dhall

in    { executables =
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
