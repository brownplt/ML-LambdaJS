#!/bin/bash

P=`dirname $0`

for FILE in $P/Tests/*/*.js
do
  $P/../../build/jsc.d.byte $P/Tests/shell.js `dirname $FILE`/shell.js $FILE
done
