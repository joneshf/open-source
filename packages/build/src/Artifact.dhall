    let Dependency = ./Dependency.dhall

in  let Artifact
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
              }
          >

in  Artifact
