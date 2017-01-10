#!/bin/bash

dir=examples/advent2016

for i in $(ls $dir | grep '^[0-9]*$' | sort -n)
do
    diff \
        <(set -x ; time stack exec pure-vanilla interact $dir/$i <$dir/$i.input) \
        $dir/$i.output
done
