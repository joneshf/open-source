    let Manifest = ./../../Manifest.dhall

in  let Package = ./../../Package.dhall

in  let Test = ./../../Test.dhall

in    { manifest =
          (constructors Manifest).Cabal {=}
      , name =
          "dhall-javascript"
      , sourceDirectory =
          "haskell/src"
      , tests =
          [] : List Test
      , version =
          "0.1.0.0"
      }
    : Package
