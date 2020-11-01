let Dependency = ./Dependency.dhall

let Target = ./Target.dhall

let Artifact
    : Type
    = < PureScript/program :
          { compiler :
              Text
          , dependencies :
              List Dependency
          , main :
              { file : Text, module : Text }
          , name :
              Text
          , src :
              Text
          , target :
              Target
          }
      >

in  Artifact
