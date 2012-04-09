#!/bin/bash

sbt doc
git checkout gh-pages
cp -r target/scala-2.9.1/api/* ./
git add .
git commit -m "updated api docs"
git push
git checkout master
