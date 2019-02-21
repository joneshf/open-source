# go-pretty

A pretty printer package written in go.

## Motivation

Rendering simple text is easy: you concatenate strings.
Rendering non-trivial text is not quite so easy.
If you wanted to render something like:

```go
func pos(x int) bool { return x > 0 }
```

You could try and write something to render it, and it would probably work.

What happens when the line grows beyond a decent amount?
It'd be nice if rendering just sort of took care of itself:

```go
func pos(x int) string {
  if x > 0 {
    return "positive"
  }
  return "not positive"
}
```

Trying to write the logic to do that imperatively by hand is hard.
This package seeks to make solving this problem easier!

## Usage

There are two steps to using this package: describe the fully expanded text,
render the text with a maximum width.
If a chunk of text will fit within the width when rendering,
it's collapsed to fit on the same line.

## Examples

Soon...

## References

The work in this package is based on [A prettier printer][] by Philip Wadler.

[a prettier printer]: https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
