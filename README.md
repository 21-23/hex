# hex
Service orchestration and scheduling

## Running tests

There are two test suites: unit and integration. By default

    $ stack test

will run only unit tests.

To run all tests, including integration tests, pass additional flag

    $ stack test --flag hex:integration-tests

