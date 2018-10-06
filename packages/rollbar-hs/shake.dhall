    let Manifest = ./../../Manifest.dhall

in  let Package = ./../../Package.dhall

in    { manifest =
          (constructors Manifest).Hpack {=}
      , name =
          "rollbar-hs"
      , tests =
          [ "doc-test" ]
      , version =
          "0.3.1.0"
      }
    : Package
