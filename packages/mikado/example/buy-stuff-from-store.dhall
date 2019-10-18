let dependency = ../dependency.dhall

in  dependency.complete
    { dependencies =
        [] : List dependency.dependency
    , description =
        "Buy stuff from the store"
    }
