let List/concat =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/8098184d17c3aecc82674a7b874077a7641be05a/Prelude/List/concat sha256:54e43278be13276e03bd1afa89e562e94a0a006377ebea7db14c7562b0de292b

let List/concatMap =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/8098184d17c3aecc82674a7b874077a7641be05a/Prelude/List/concatMap sha256:3b2167061d11fda1e4f6de0522cbe83e0d5ac4ef5ddf6bb0b2064470c5d3fb64

let List/map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/8098184d17c3aecc82674a7b874077a7641be05a/Prelude/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let Text/concatSep =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/8098184d17c3aecc82674a7b874077a7641be05a/Prelude/Text/concatSep sha256:e4401d69918c61b92a4c0288f7d60a6560ca99726138ed8ebc58dca2cd205e58

let dependency : Type = (./dependency.dhall).dependency

let description
    : dependency → Text
    =   λ(x : dependency)
      → x
        Text
        (   λ(x : { dependencies : List Text, description : Text })
          → x.description
        )
        (   λ(x : { dependencies : List Text, description : Text })
          → x.description
        )

let graphviz-dependency
    : { dependencies : List (List Text), description : Text } → List Text
    =   λ(x : { dependencies : List (List Text), description : Text })
      →   [ Text/show x.description ]
        # List/concatMap
          (List Text)
          Text
          (   λ(dependencies : List Text)
            → List/map
              Text
              Text
              (   λ(dependency : Text)
                → Text/show x.description ++ " -> " ++ dependency
              )
              dependencies
          )
          x.dependencies

let graphviz-dependencies
    : dependency → List Text
    = λ(x : dependency) → x (List Text) graphviz-dependency graphviz-dependency

let graphviz-nodes
    : dependency → List Text
    =   λ(x : dependency)
      → x
        (List Text)
        (   λ(x : { dependencies : List (List Text), description : Text })
          →   [ Text/show x.description ++ " [color = gray; fontcolor = gray]" ]
            # List/concat Text x.dependencies
        )
        (   λ(x : { dependencies : List (List Text), description : Text })
          → [ Text/show x.description ] # List/concat Text x.dependencies
        )

let graphviz
    : dependency → Text
    =   λ(x : dependency)
      → ''
        digraph {
          concentrate = true;
          ${Text/show (description x)} [peripheries = 2];
          ${Text/concatSep "\n  " (graphviz-nodes x)}
          ${Text/concatSep "\n  " (graphviz-dependencies x)}
        }
        ''

in  { graphviz = graphviz }
