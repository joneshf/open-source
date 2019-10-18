let List/map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/8098184d17c3aecc82674a7b874077a7641be05a/Prelude/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let dependency
    : Type
    =   ∀(dependency : Type)
      → ∀ ( complete
          : { dependencies : List dependency, description : Text } → dependency
          )
      → ∀ ( incomplete
          : { dependencies : List dependency, description : Text } → dependency
          )
      → dependency

let complete
    : { dependencies : List dependency, description : Text } → dependency
    =   λ(x : { dependencies : List dependency, description : Text })
      → λ(dependency : Type)
      → λ ( complete
          : { dependencies : List dependency, description : Text } → dependency
          )
      → λ ( incomplete
          : { dependencies : List dependency, description : Text } → dependency
          )
      → complete
        { dependencies =
              List/map
              dependency@1
              dependency
              (λ(x : dependency@1) → x dependency complete incomplete)
              x.dependencies
            : List dependency
        , description =
            x.description
        }

let incomplete
    : { dependencies : List dependency, description : Text } → dependency
    =   λ(x : { dependencies : List dependency, description : Text })
      → λ(dependency : Type)
      → λ ( complete
          : { dependencies : List dependency, description : Text } → dependency
          )
      → λ ( incomplete
          : { dependencies : List dependency, description : Text } → dependency
          )
      → incomplete
        { dependencies =
              List/map
              dependency@1
              dependency
              (λ(x : dependency@1) → x dependency complete incomplete)
              x.dependencies
            : List dependency
        , description =
            x.description
        }

in  { complete = complete, dependency = dependency, incomplete = incomplete }
