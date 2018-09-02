#!/usr/bin/env bash

for tf in *-test.rkt; do
  echo "* run test ${tf}"
  racket -t "${tf}"
done
