# pagure-haskell

This is a rewrite of `pagure-haskell` using the awesome Servant library for
Haskell.

Using this library means added type safety, but also free API documentation
generation with examples, and the ability to generate API clients in other
languages very easy using Servant's built in code-gen facilities.

# `git-pagure`

In this project is also a `git-pagure` binary which is a commandline client
for interacting with pagure. It is a rewrite of `pagure-cli` using the new
Servant implementation.

# License

BSD-2. (c) 2016 Red Hat, Inc. See LICENSE for details.
