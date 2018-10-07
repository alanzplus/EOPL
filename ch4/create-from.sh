#!/usr/bin/env bash
set -e

if (($# != 2)); then
    echo 'Usage: create-from $SOUNCE_NAME $TARGET_NAME'
    exit -1
fi

readonly source=$1
readonly target=$2

create-from() {
    local readonly src=$1
    local readonly dst=$2
    for fn in "${src}"*; do 
        if [[ $fn =~ .*-test.* ]]; then
            cp $fn "${dst}-test.rkt"
            sed -i s/"${src}"/"${dst}"/g "${dst}-test.rkt"
        else
            cp $fn "${dst}.rkt"
            sed -i s/"${src}"/"${dst}"/g "${dst}.rkt"
        fi
    done
}

echo "* Executing create-from ${source}-spec ${target}-spec"
create-from "${source}-spec" "${target}-spec"

echo "* Running ${target}-spec-test.rkt"
racket -t "${target}-spec-test.rkt"

echo "* Executing create-from ${source}-interpreter ${target}-interpreter"
create-from "${source}-interpreter" "${target}-interpreter"

echo "* Running ${target}-interpreter-test.rkt"
racket -t "${target}-interpreter-test.rkt"
