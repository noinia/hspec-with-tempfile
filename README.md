# Hspec-with-tempfile

This package provides a (hopefully) convenient way of running golden
tests that use temporary files using hspec.

It is fairly configurable, but the main idea is that the output files
are saved only if they differ from the expected (golden) output.
