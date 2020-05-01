#! /usr/bin/env bash

./millw unidoc
./millw examples.mdoc
cd site
git clone "https://github.com/satabin/swam.git" --branch gh-pages output
rm -rf output/*
nanoc
cd output
git add .
git commit -am "Upgrade website"
git push --force --quiet "https://${GH_TOKEN}@github.com/satabin/swam.git" gh-pages> /dev/null 2>&1
