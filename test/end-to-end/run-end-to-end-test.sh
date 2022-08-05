#!/bin/bash

OUT_FILE='/tmp/rosen-test-out.txt'
stack build
cat test-commands.txt | stack exec rosen-exe > "$OUT_FILE" 2> /dev/null
if diff "$OUT_FILE" "./expected-results.txt"
then
    echo "Test passed"
    exit 0
else
    echo "Test failed"
    exit 1
fi
