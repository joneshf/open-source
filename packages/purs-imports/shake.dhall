let Package = ./../../Package.dhall

in    Package.Go
      { bin =
          [ { file = ".", name = "purs-imports" } ]
      , name =
          "purs-imports"
      , sourceDirectory =
          "."
      , version =
          "1.0.0"
      }
    : Package
