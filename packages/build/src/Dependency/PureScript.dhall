    let PureScript/FileModule : Type = { file : Text, module : Text }

in  let PureScript
        : Type
        = { modules :
              List PureScript/FileModule
          , name :
              Text
          , src :
              Text
          , uri :
              Text
          , version :
              Text
          }

in  { PureScript = PureScript, PureScript/FileModule = PureScript/FileModule }
