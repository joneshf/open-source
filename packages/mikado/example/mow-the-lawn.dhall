let dependency = ../dependency.dhall

in  dependency.incomplete
    { dependencies =
        [ ./check-lawn-mower.dhall, ./buy-stuff-from-store.dhall ]
    , description =
        "Mow the lawn"
    }
