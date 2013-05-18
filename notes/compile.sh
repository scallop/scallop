#!/bin/bash

for t in $(find . -iname "*.m4" | grep -v macro.m4); do
    cat $t | m4 > ${t/.m4/}
done
