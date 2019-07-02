# purs-imports

A CLI that generates a [Graphviz][] graph from a [PureScript][] module.

## Usage

Let's say we've got a [PureScript][] module named `Main.purs`:

```PureScript
module Main (main) where

import Prelude

import Effect as Effect

main :: Effect.Effect Unit
main = pure unit
```

If we run `purs-imports` on `Main.purs`:

```sh
$ purs-imports Main.purs
```

We should get a graph like:

```Graphviz
digraph imports {
  "Main" -> "Effect";
  "Main" -> "Prelude";
}
```

[graphviz]: https://graphviz.org
[purescript]: http://purescript.org
