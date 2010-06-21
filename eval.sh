#!/bin/bash

if [ $# -ne 1 ]
then
  echo "Usage: `basename $0` PATH"
  echo
  echo "If PATH is a directory, we evaluate all .js files in PATH and its"
  echo "sub-directories."
  exit 1
fi

P=`dirname $0`
BASE=$1

tests=0
fails=0

function run_test {
  tests=`expr $tests + 1`
  $P/build/jsc.d.byte  -file $1 -env $P/data/ecma262-3.lambdajs -eval \
    || fails=`expr $fails + 1`
}

if [ -d $BASE ]; then
  FILES=`find $BASE -name *.js`
  for FILE in $FILES; do
    run_test $FILE
  done
elif [ -f $BASE ]; then
  run_test $BASE
else
  echo "$BASE does not exist."
  exit 1
fi

echo "$tests total files. Failures on $fails files."
