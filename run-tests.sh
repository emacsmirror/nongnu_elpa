#!/bin/bash

export EMACS_TEST_JUNIT_REPORT=1
$EMACS --batch -L . -L tests -l test-flymake-pyrefly \
       -f ert-run-tests-batch-and-exit
