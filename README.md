# open-source

[![CircleCI](https://circleci.com/gh/joneshf/open-source.svg?style=svg)](https://circleci.com/gh/joneshf/open-source)

A single repo for all of my open source stuff.

## Setup

Two tools are used to bootstrap this repo: [`nix`][] and [`direnv`][].
Once those tools are setup, everything else should work seamlessly.

N.B. Neither tool is actually required.
Everything in this repo _probably_ could work without either.
But, any workflow without both is not currently supported.

### nix

[`nix`][] is used to make working in the repo reproducible.

When I come back to the repo in a few days, weeks, months, years,
I don't want to have to remember how to get the environment set up properly.
If someone new comes to the repo,
they shouldn't have to jump through hoops to make it work on their computer.

See the [install instructions][nix install] for setting up [`nix`][].

After it is setup, it should be enough to launch the shell: `nix-shell`.

### direnv

[`direnv`][] is used to make working with [`nix`][] a little easier.

Without [`direnv`][], there is a step that has to happen each time to start working:
`nix-shell`.
It's not a huge burden, but it's something extra to remember.
It also takes a bit of time once the environment gets large enough.
[`direnv`][] caches the environment of the shell and can automatically spin up the
environment when the directory is entered.
This means that working with the project after the initial setup is seamless,
and more efficient.

See the [install instructions][direnv install] for setting up [`direnv`][].

After it is setup, it should be enough to allow it to work: `direnv allow`.

## Development

[`shake`][] is used to build everything in this repo.
Installation of [`shake`][] is taken care of by [`nix`][].

Everything can be built from the top-level:

```sh
$ shake build
```

The default rule is `build`. The above is equivalent to:

```sh
$ shake
```

Tests can be run from the top-level as well:

```sh
$ shake test
```

Individual packages can be watched for changes.
For example, for the `rollbar-hs` package:

```sh
$ shake watch-rollbar-hs
```

Packages can be uploaded to hackage if they are unrealeased:

```sh
$ shake upload-to-hackage
```

This rule is safe to run at any time.
It will only upload if the version doesn't exist.
It will do nothing if the version already exists.

## Structure

### Packages

Currently,
each package defines a small bit of configuration in a `shake.dhall` file.
The types for each `shake.dhall` file are generated in the top-level
when [`shake`][] is run.
N.B. To avoid drift between this README and the current state of the world,
those file names are not explicitly mentioned here.

[`Dhall`][] is used as the configuration language for a few of reasons:
1. The language is expressive enough to capture most configuration.
    There are both sums and products,
    so much of the hardness with JSON/YAML goes away.
1. We can off-load most of the knowledge about each field
    and alternate to the type system.
    The type of a package can be viewed as a schema, in that sense.
    We can avoid prose to attempt to document what the fields are.
1. We can easily map the configuration into [`shake`][].
    We avoid thinking about how to map Haskell ADTs to a language without sums.
1. The configuration can stay consistent across the entire repository.
    It's very easy to have an explosion of ideas that become unmanageable.
    In an un-typed configuration,
    it's harder to know whether something is used or not.
    If one package needs a different kind of configuration and changes the type,
    every package should be aware of the change and stay in line.

The `shake.dhall` files are read in by the `Shakefile.hs` file at start up,
and rules are made from them.
This allows the build system to grow as the repo grows,
but doesn't force intimate knowledge of [`shake`][] into every area of the repo.

To add another package,
it should be enough to create a directory, and add a `shake.dhall` file.
It should be picked up by the `Shakefile.hs` file, and start working immediately.

## License

Unless otherwise specified,
everything here is licensed under the license located in the top-level directory.

[`Dhall`]: https://dhall-lang.org
[`direnv`]: https://github.com/direnv/direnv
[`nix`]: https://nixos.org
[`shake`]: https://shakebuild.com/
[direnv install]: https://github.com/direnv/direnv#install
[nix install]: https://nixos.org/nix/
