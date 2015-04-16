#! /usr/bin/env bash

docker run \
  -v `pwd`:/home/gusdev/hurling \
  images.reesd.com/reesd/stack \
  cabal install hurling/hurling.cabal
