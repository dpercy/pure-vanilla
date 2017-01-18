#!/bin/bash

dir=examples/advent2016

test_advent() {
    i="$1"
    diff \
        <(set -x ; time stack exec pure-vanilla interact $dir/$i <$dir/$i.input) \
        $dir/$i.output
}

if [ $# -eq 0 ]; then
    for i in $(ls $dir | grep '^[0-9]*$' | sort -n)
    do
        test_advent "$i"
    done
else
    for i in "$@"
    do
        test_advent "$i"
    done
fi
