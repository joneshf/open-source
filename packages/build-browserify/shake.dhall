let Package = ./../../Package.dhall

in    Package.JavaScript
      { bin =
          [ { file =
                "src/index.js"
            , name =
                "build-browserify"
            , node-version =
                "10.15.3"
            }
          ]
      , dependencies =
          [ { package = "browserify", version = "16.2.3" }
          , { package = "nexe", version = "3.0.0-beta.15" }
          ]
      , license =
          "MIT"
      , name =
          "build-browserify"
      , sourceDirectory =
          "src"
      , version =
          "1.0.0"
      }
    : Package
