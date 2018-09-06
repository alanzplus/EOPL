#!/usr/bin/env bash

for i in *-test.rkt; do
  sed -i -e "s/exe\(.*\)/\1/" $i
done
