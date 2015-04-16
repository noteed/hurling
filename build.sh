#! /usr/bin/env bash

docker run \
  -v `pwd`/../minicron:/home/gusdev/minicron \
  -v `pwd`/../humming:/home/gusdev/humming \
  -v `pwd`:/home/gusdev/hurling \
  images.reesd.com/reesd/stack \
  cabal install minicron/minicron.cabal humming/humming.cabal hurling/hurling.cabal
