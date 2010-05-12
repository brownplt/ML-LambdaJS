#!/bin/bash

P=`dirname $0`

echo "****************************************"
echo ".js tests:"
echo ""

for FILE in $P/*.js
do
    $P/../../src/jsc $FILE -env $P/test_env -env $P/../../data/es5-lib.es5 -full-desugar -eval
done

echo ""
echo "****************************************"
echo "es5 tests:"

for FILE in $P/*.es5
do
    $P/../../src/jsc $FILE -env $P/test_env -env $P/../../data/es5-lib.es5 -full-desugar -eval
done

