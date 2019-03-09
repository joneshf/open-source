let Manifest = ./../../Manifest.dhall

let Package = ./../../Package.dhall

let Test = ./../../Test.dhall

in    { executables =
          [ { executableDirectory =
                "haskell/app"
            , executableName =
                "dhall-to-javascript"
            }
          ]
      , manifest =
          Manifest.Cabal {=}
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
