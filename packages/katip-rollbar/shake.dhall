    let Manifest = ./../../Manifest.dhall

in  let Package = ./../../Package.dhall

in  let Test = ./../../Test.dhall

in    { manifest =
          (constructors Manifest).Hpack {=}
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
