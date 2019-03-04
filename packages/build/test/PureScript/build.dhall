    let artifact = constructors ./../../src/Artifact.dhall

in  let dependencies = ./../../src/dependencies.dhall

in  [ artifact.PureScript/program
      { compiler =
          "0.12.3"
      , dependencies =
            dependencies.purescript-console-4-2-0
          # dependencies.purescript-control-4-1-0
      , main =
          { file = "Main.purs", module = "Main" }
      , name =
          "echo"
      , src =
          "."
      }
    ]
