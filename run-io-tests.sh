#!/bin/bash

# This script runs an executable file (the first argument) with input from the
# files in the tests folder (second argument) and prints out the test results.
# The files in the tests folder must come in pairs, named testname.in and
# testname.expect for each testname. The contents of testname.in are supplied
# as input to the executable file, and testname.expect contains the expected
# output.

# example usage:
#  $ ls
#  example-prog    example-tests   run-io-tests.sh
#  $ ls example-tests
#  test1.expect    test1.in        test2.expect    test2.in
#  $ cat example-tests/test2.expect
#  a phrase and a newline
#  $ ./run-io-tests.sh ./example-prog example-tests
#  *** Test "test1" passed. ***
#  *** Test "test2" failed. ***
#  produced output:
#  a phrase and a newline---
#  expected output:
#  a phrase and a newline
#  ---
#  $

program="$1"
tests="$2"

# verify tests folder contains no directories
for i in "${tests}/"*; do
  if [[ -d $i ]]; then
    echo "Tests not run: $tests contains directories"
    exit 0
  fi
done

# verify tests folder is not empty
if [ $( find "$tests" | grep -c "$tests") = 1 ]; then
  echo "Tests not run: no tests to run"
  exit 0
fi

# verify tests folder contains properly matching files
for i in "${tests}/"*; do
  if [[ $i = *.in ]]; then
    if [[ -e ${i%.in}.expect ]]; then
      :
    else
      echo "Tests not run: missing file ${i%.in}.expect"
      exit 0
    fi
  elif [[ $i = *.expect ]]; then
    if [[ -e ${i%.expect}.in ]]; then
      :
    else
      echo "Tests not run: missing file ${i%.expect}.in"
      exit 0
    fi
  else
    echo "Tests not run: unexpected file $i"
    exit 0
  fi
done

# run the tests
echo "---" >outputend.txt
for i in "${tests}/"*.in; do
  testfilename="${i#${tests}/}"
  testname="${testfilename%.in}"
  in="$i"
  out="${i%.in}.out"
  expect="${i%.in}.expect"
  compare="${i%.in}.compare"
  "$program" <"$in" >"$out"
  diff "$out" "$expect" >"$compare"
  if [ $( grep -c ^ "$compare" ) = 0 ]; then
    outcome="passed."
  else
    outcome="failed."
  fi

  echo "*** Test \"${testname}\" $outcome ***"
  if [ "$outcome" = "failed." ]; then
    echo produced output:
    cat "$out" outputend.txt
    echo expected output:
    cat "$expect" outputend.txt
  fi

  rm "$out"
  rm "$compare"
done
rm outputend.txt

