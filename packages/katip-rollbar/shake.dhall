    let Manifest = ./../../Manifest.dhall

in  let Package = ./../../Package.dhall

in    { manifest =
          (constructors Manifest).Hpack {=}
      , name =
          "katip-rollbar"
      , tests =
          [] : List Text
      , version =
          "0.3.0.0"
      }
    : Package
