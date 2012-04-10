#!/bin/bash

sbt doc
git checkout gh-pages
rm -r index
rm -r org
cp -r target/scala-2.9.1/api/* ./
git add .
git commit -m "updated api docs"
git push
git checkout master
