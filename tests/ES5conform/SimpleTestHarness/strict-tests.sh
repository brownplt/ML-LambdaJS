#!/bin/bash

P=`dirname $0`
failures=$((0))
numTests=$((0))

function failure {
    retcode=$?
    if [[ $retcode == 1 ]]; then
        failures=$((failures+1))
    fi
}

for FILE in `find ../TestCases/ -type f -name "*.js"`
do
    numTests=$((numTests+1))
    $P/ljs-test-single.sh $FILE || failure
done

echo "$numTests tried.  $failures failed."
