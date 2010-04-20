#!/bin/bash

P=`dirname $0`

for FILE in $P/*.js
do
  $P/../../build/jsc.d.byte -file $FILE -js
done