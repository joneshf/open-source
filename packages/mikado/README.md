# mikado

Define [Mikado graph][]s in [Dhall][]

## Motivation

Defining [Mikado graph][]s in code is pretty hard:

* It's fairly easy to accidentally get a cycle in the graph.
* It's equally likely to end up with a dependency not part of the graph.
* Managing larger graphs is kind of hard.

We can lean on [Dhall][] to define the dependencies as expressions, and build the graph from the expressions.

[Dhall][] disallows general recursion, so an expression cannot refer to itself.
In this case, it means we can never have cycles in our graph.

Since the expression is the graph, we cannot end up with detached dependencies.
If something is in the graph, it always has a path from the goal.

[Dhall][] allows importing of arbitrary expressions, so we can take a graph and split it across multiple files once it starts to get complex.

## Usage

1. Define a graph:

    ```Dhall
    -- mow-the-lawn.dhall
    let package = https://raw.githubusercontent.com/joneshf/open-source/master/packages/mikado/package.dhall

    let dependency = package.dependency

    let buy-stuff-from-store =
          dependency.complete
          { dependencies =
              [] : List dependency.dependency
          , description =
              "Buy stuff from the store"
          }

    let check-lawn-mower =
          dependency.complete
          { dependencies =
              [ buy-stuff-from-store ]
          , description =
              "Check the lawn mower"
          }

    in  dependency.incomplete
        { dependencies =
            [ check-lawn-mower, buy-stuff-from-store ]
        , description =
            "Mow the lawn"
        }
    ```

1. Render it to Graphviz:

    ```console
    $ dhall text <<< 'let package = https://raw.githubusercontent.com/joneshf/open-source/master/packages/mikado/package.dhall in package.render.graphviz ./mow-the-lawn.dhall' > mow-the-lawn.gv
    ```

1. Render the Graphviz to a format (like PNG):

    ```console
    $ dot -T png -o mow-the-lawn.png mow-the-lawn.gv
    ```

1. Rejoice in your easy to maintain [Mikado graph][].


See a more complete example in the example directory.

[dhall]: https://dhall-lang.org/
[mikado graph]: https://pragprog.com/magazines/2010-06/the-mikado-method
