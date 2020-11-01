let concatSep =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/9203c77bfa8f96b3bc9a26016da94d27537d13b9/Prelude/Text/concatSep sha256:bc088ae2be57a70df3f1cc906f02a9c16c2c486b13dbc7f2a927c4fc83d4f517

let Dependency : Type = ./Dependency.dhall

let PureScript/FileModule
    : Type
    = (./Dependency/PureScript.dhall).PureScript/FileModule

let purescript
    :   Text
      → List PureScript/FileModule
      → List Dependency
      → Text
      → List Dependency
    =   λ ( repo
          : Text
          )
      → λ(modules : List PureScript/FileModule)
      → λ(dependencies : List Dependency)
      → λ(version : Text)
      →   dependencies
        # [ Dependency.PureScript
            { modules =
                modules
            , name =
                repo
            , src =
                "src"
            , uri =
                "https://github.com/purescript/${repo}/archive/${version}/.tar.gz"
            , version =
                version
            }
          ]

let fileModule
    : List Text → PureScript/FileModule
    =   λ(names : List Text)
      → { file = concatSep "/" names ++ ".purs", module = concatSep "." names }

let purescript-prelude-4-1-0
    : List Dependency
    = purescript
      "purescript-prelude"
      [ fileModule [ "Control", "Applicative" ]
      , fileModule [ "Control", "Apply" ]
      , fileModule [ "Control", "Bind" ]
      , fileModule [ "Control", "Category" ]
      , fileModule [ "Control", "Monad" ]
      , fileModule [ "Control", "Semigroupoid" ]
      , fileModule [ "Data", "Boolean" ]
      , fileModule [ "Data", "BooleanAlgebra" ]
      , fileModule [ "Data", "Bounded" ]
      , fileModule [ "Data", "CommutativeRing" ]
      , fileModule [ "Data", "DivisionRing" ]
      , fileModule [ "Data", "Eq" ]
      , fileModule [ "Data", "EuclideanRing" ]
      , fileModule [ "Data", "Field" ]
      , fileModule [ "Data", "Function" ]
      , fileModule [ "Data", "Functor" ]
      , fileModule [ "Data", "HeytingAlgebra" ]
      , fileModule [ "Data", "Monoid" ]
      , fileModule [ "Data", "Monoid", "Additive" ]
      , fileModule [ "Data", "Monoid", "Conj" ]
      , fileModule [ "Data", "Monoid", "Disj" ]
      , fileModule [ "Data", "Monoid", "Dual" ]
      , fileModule [ "Data", "Monoid", "Endo" ]
      , fileModule [ "Data", "Monoid", "Multiplicative" ]
      , fileModule [ "Data", "NaturalTransformation" ]
      , fileModule [ "Data", "Ord" ]
      , fileModule [ "Data", "Ord", "Unsafe" ]
      , fileModule [ "Data", "Ordering" ]
      , fileModule [ "Data", "Ring" ]
      , fileModule [ "Data", "Semigroup" ]
      , fileModule [ "Data", "Semigroup", "First" ]
      , fileModule [ "Data", "Semigroup", "Last" ]
      , fileModule [ "Data", "Semiring" ]
      , fileModule [ "Data", "Show" ]
      , fileModule [ "Data", "Symbol" ]
      , fileModule [ "Data", "Unit" ]
      , fileModule [ "Data", "Void" ]
      , fileModule [ "Prelude" ]
      , fileModule [ "Record", "Unsafe" ]
      , fileModule [ "Type", "Data", "Row" ]
      , fileModule [ "Type", "Data", "RowList" ]
      ]
      ([] : List Dependency)
      "v4.1.0"

let purescript-control-4-1-0
    : List Dependency
    = purescript
      "purescript-control"
      [ fileModule [ "Control", "Alt" ]
      , fileModule [ "Control", "Alternative" ]
      , fileModule [ "Control", "Comonad" ]
      , fileModule [ "Control", "Extend" ]
      , fileModule [ "Control", "Lazy" ]
      , fileModule [ "Control", "MonadPlus" ]
      , fileModule [ "Control", "MonadZero" ]
      , fileModule [ "Control", "Plus" ]
      , fileModule [ "Data", "Monoid", "Alternate" ]
      ]
      purescript-prelude-4-1-0
      "v4.1.0"

let purescript-effect-2-0-1
    : List Dependency
    = purescript
      "purescript-effect"
      [ fileModule [ "Effect" ]
      , fileModule [ "Effect", "Class" ]
      , fileModule [ "Effect", "Uncurried" ]
      , fileModule [ "Effect", "Unsafe" ]
      ]
      purescript-prelude-4-1-0
      "v2.0.1"

let purescript-console-4-2-0
    : List Dependency
    = purescript
      "purescript-console"
      [ fileModule [ "Effect", "Class", "Console" ]
      , fileModule [ "Effect", "Console" ]
      ]
      (purescript-effect-2-0-1 # purescript-prelude-4-1-0)
      "v4.2.0"

in  { purescript-console-4-2-0 =
        purescript-console-4-2-0
    , purescript-control-4-1-0 =
        purescript-control-4-1-0
    , purescript-effect-2-0-1 =
        purescript-effect-2-0-1
    , purescript-prelude-4-1-0 =
        purescript-prelude-4-1-0
    }
