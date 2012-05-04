#!/bin/bash

sbt doc
for x in $(find target/scala-2.9.1/api/ -type f); do sed -i "s_`pwd`/__" $x; done
git checkout gh-pages
rm -r index
rm -r org
cp -r target/scala-2.9.1/api/* ./
git add .
git commit -m "updated api docs"
git push
git checkout master
