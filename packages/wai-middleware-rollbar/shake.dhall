    let Manifest = ./../../Manifest.dhall

in  let Package = ./../../Package.dhall

in  let Test = ./../../Test.dhall

in    { manifest =
          (constructors Manifest).Cabal {=}
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
