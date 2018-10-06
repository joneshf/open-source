    let Manifest = ./../../Manifest.dhall

in  let Package = ./../../Package.dhall

in    { manifest =
          (constructors Manifest).Cabal {=}
      , name =
          "wai-middleware-rollbar"
      , tests =
          [] : List Text
      , version =
          "0.11.0"
      }
    : Package
