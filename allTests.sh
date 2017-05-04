#!/bin/bash

./node_modules/.bin/elm-doc-test &&
    ./node_modules/.bin/elm test tests/Doc/Main.elm &&
    (cd spec-tests && ./runTests.sh)
