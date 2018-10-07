    let Manifest = ./../../Manifest.dhall

in  let Package = ./../../Package.dhall

in    { manifest =
          (constructors Manifest).Hpack {=}
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
