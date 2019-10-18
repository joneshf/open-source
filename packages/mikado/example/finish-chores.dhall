let dependency = ../dependency.dhall

in  dependency.incomplete
    { dependencies =
        [ ./buy-stuff-from-store.dhall
        , ./mow-the-lawn.dhall
        , ./prune-bush.dhall
        ]
    , description =
        "Finish chores"
    }
