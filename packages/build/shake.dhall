    let Manifest = ./../../Manifest.dhall

in  let Package = ./../../Package.dhall

in  let Test = ./../../Test.dhall

in    { executables =
          [ { executableDirectory = "src", executableName = "build" } ]
      , manifest =
          (constructors Manifest).Cabal {=}
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
