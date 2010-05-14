#!/bin/bash

P=`dirname $0`
FILE=$1

$P/../../src/jsc $FILE -env $P/test_env -env $P/../../data/es5-lib.es5 -full-desugar -pretty -eval
