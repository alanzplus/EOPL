#!/usr/bin/env bash
set -e

if (($# != 3)); then
    echo 'Usage: create-from $SOURCE_DIR $SOUNCE_NAME $TARGET_NAME'
    exit -1
fi

readonly source_dir=$1
readonly source=$2
readonly target=$3

# Fix bug of spec reference in interpreter
create-from() {
    local readonly src=$1
    local readonly dst=$2
    for fn in "${source_dir}/${src}"*; do 
        if [[ $fn =~ .*-test.* ]]; then
            sed s/"${src}"/"${dst}"/g "${fn}" > "${dst}-test.rkt"
        else
            sed s/"${src}"/"${dst}"/g "${fn}" > "${dst}.rkt"
        fi
    done
}

echo "* Executing create-from ${source}-spec ${target}-spec"
create-from "${source}-spec" "${target}-spec"

echo "* Running ${target}-spec-test.rkt"
racket -t "${target}-spec-test.rkt"

echo "* Executing create-from ${source}-interpreter ${target}-interpreter"
create-from "${source}-interpreter" "${target}-interpreter"
sed -i s/"${source}-spec"/"${target}-spec/g" "${target}-interpreter.rkt"

echo "* Running ${target}-interpreter-test.rkt"
racket -t "${target}-interpreter-test.rkt"
