#! /bin/sh
git fetch spec-testsuite master

git subtree pull --prefix runtime/test/resources/spec-test spec-testsuite master --squash
