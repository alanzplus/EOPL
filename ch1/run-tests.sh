#!/usr/bin/env bash

if [[ $# = 0 ]]; then
  for tf in *-test.rkt; do
    echo "* run test ${tf}"
    racket -t "${tf}"
  done
else
  echo "* run test $1"
  racket -t "$1"
fi

