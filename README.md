# Hspec-with-tempfile

![GitHub Workflow Status](
https://img.shields.io/github/actions/workflow/status/noinia/hspec-with-tempfile/gettested.yml?branch=main)
[![Hackage](https://img.shields.io/hackage/v/hspec-with-tempfile.svg?color=success)](https://hackage.haskell.org/package/hspec-with-tempfile)
[![API docs coverage](https://img.shields.io/endpoint?url=https%3A%2F%2Fnoinia.github.io%2Fhspec-with-tempfile%2Fhaddock_badge.json)](https://noinia.github.io/hspec-with-tempfile/haddocks)


This package provides a (hopefully) convenient way of running golden
tests that use temporary files using hspec.

It is fairly configurable, but the main idea is that the output files
are saved only if they differ from the expected (golden) output.

## Example

Here is some example output:

```
Example
  example test
    fib_5.golden [✔]
      golden test succeeded
    fib_10.golden [✘]
      golden test failed

Failures:

  /tmp/fib_10110142-1.golden:0:0:
  1) Example, example test, fib_10.golden
       golden test with output 55 failed since Diff {expected = "5", actual = "55"}

```
