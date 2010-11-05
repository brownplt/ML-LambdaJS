#!/bin/bash

P=`dirname $0`
FILE=$1
output=`mktemp -t ljses5.XXXX`
errOutput=`mktemp -t ljses5err.XXXX`

$P/../../../src/jsc $P/ljs_harness.js $FILE $P/ljs-run.js -env $P/../../../data/es5-lib.es5 -full-desugar -eval > $output 2> $errOutput

failed1=`grep Failed $output`
failed2=`grep failed $output`
failed3=`grep exception $errOutput`
failed="$failed1$failed2$failed3"
if [[ $failed != "" ]]; 
then
    exit 1;
fi
exit 0;
