#!/bin/bash

P=`dirname $0`

$P/build/jsc.d.byte $@ -env $P/data/ecma262-3.lambdajs
