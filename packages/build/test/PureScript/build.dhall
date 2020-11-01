let Artifact = ./../../src/Artifact.dhall

let dependencies = ./../../src/dependencies.dhall

let Target = ./../../src/Target.dhall

let program =
        λ(name : Text)
      → λ(target : Target)
      → { compiler =
            "0.12.3"
        , dependencies =
              dependencies.purescript-console-4-2-0
            # dependencies.purescript-control-4-1-0
        , main =
            { file = "Main.purs", module = "Main" }
        , name =
            name
        , src =
            "."
        , target =
            target
        }

in  [ Artifact.PureScript/program
      (program "echo.browser.js" (Target.Browser {=}))
    , Artifact.PureScript/program (program "echo.node.js" (Target.NodeJS {=}))
    ]
