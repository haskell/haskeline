cd tests # one of the tab completion tests is completing the dummy- dir in tests
stack exec haskeline-tests $(stack path --local-install-root)/bin/haskeline-examples-Test
