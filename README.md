## About

`hspec-snap` is a library to allow you to write tests against Snap web
applications in the context of an `hspec` test suite.

## Usage

The basic usage is that within an `hspec` test suite, you can insert a
block with `snap`, within which all of the normal spec organization
tools work (`describe`, `context`, and `it`), but the content of the
actual tests should be tests in the Snap context. These tests can make
requests against your application, run arbitrary handler functions,
and assert various HTTP statuses, content of HTML pages, or just plain
unit tests. You can't mix in other types of tests within the `snap`
block, and you can't share values between different unit tests (a unit
test being what's inside an `it`), but you can have any number of
`snap` blocks, and can make any number of assertions within a unit
test block (from HSpec's perspective, though, it is just one test).

To get started, take a look at the test suite within the `spec/`
directory.

## Examples

The easiest examples to look at are in the test suite in the `spec/`
directory.

## Tests

You can run the test suite with `cabal test`. When the robots 
last ran the suite, it: [![Circle CI](https://circleci.com/gh/dbp/hspec-snap.svg?style=svg)](https://circleci.com/gh/dbp/hspec-snap).

## License

BSD3

## Contributors

Daniel Patterson (dbp@dbpmail.net)

Tim Adams (timmy_tofu@linux.com)
