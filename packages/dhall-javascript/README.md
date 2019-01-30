# dhall-javascript

Compile [Dhall][] to JavaScript.

## Motivation

Dhall is a great language for writing configuration files.
It's also a great language for writing programs!

Dhall provides many nice [features][dhall features] for writing programs.
We can leverage those features for use in JavaScript by compiling a Dhall expression to a JavaScript expression.

## Usage

For a list of all available options, use the `--help` flag:

```sh
$ dhall-to-javascript --help
```

There are currently three ways to output JavaScript: an expression, a module, a variable.

Any valid Dhall should be accepted.
For now, we'll use a running example of Dhall:

```Dhall
\(name : Text) -> { foo = 1 + 12, bar = "Hello, " ++ name }
```

### Expression

This is the default way to use `dhall-to-javascript`.

This is the most similar to Dhall.
One use for the output is to template larger pieces of JavaScript.

```sh
$ dhall-to-javascript <<< '\(name : Text) -> { foo = 1 + 12, bar = "Hello, " ++ name }'
function ($dhall0) {
   return {bar: "Hello, " + $dhall0,foo: 13};
}
```

### Module

We can compile to a [CommonJS][]-style module.

This is useful to create an entire file ready to be consumed by [node.js][].

```sh
$ dhall-to-javascript --module <<< '\(name : Text) -> { foo = 1 + 12, bar = "Hello, " ++ name }'
module.exports = function ($dhall0) {
   return {bar: "Hello, " + $dhall0,foo: 13};
}
```

### Variable

We can compile to a named variable.

This is useful if the way you're creating the variable doesn't easily lend to templating.

```sh
$ dhall-to-javascript --var someFunction <<< '\(name : Text) -> { foo = 1 + 12, bar = "Hello, " ++ name }'
var someFunction = function ($dhall0) {
   return {bar: "Hello, " + $dhall0,foo: 13};
};
```

[commonjs]: http://www.commonjs.org/
[dhall]: https://dhall-lang.org/
[dhall features]: https://github.com/dhall-lang/dhall-lang#features
[node.js]: https://nodejs.org/
