#!/bin/bash

P=`dirname $0`

echo "****************************************"
echo ".js tests:"

for FILE in $P/*.js
do
    $P/single-test.sh $FILE
done

echo ""
echo "****************************************"
echo "es5 tests:"

for FILE in $P/*.es5
do
    $P/single-test.sh $FILE
done

