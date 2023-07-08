# Hspec-with-tempfile

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
