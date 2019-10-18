
let dependency = ../dependency.dhall

in  dependency.complete
    { dependencies =
        [ ./buy-stuff-from-store.dhall ]
    , description =
        "Prune rose bush"
    }
