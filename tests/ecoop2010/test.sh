#!/bin/bash

P=`dirname $0`

for FILE in $P/Tests/*/*.js
do
  $P/../../build/jsc.d.byte -full-desugar -env $P/../../data/ecma262-3.lambdajs -operators $P/Tests/shell.js `dirname $FILE`/shell.js $FILE 
done
