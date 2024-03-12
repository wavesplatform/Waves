#!/bin/bash

WAVES_VERSION=$(cut -d\" -f2 version.sbt)

docker run \
  -v "$PWD":/src \
  -e HOME=/opt/sbt/home \
  -w /src \
  --rm -it wavesplatform/node-sbt-builder:$WAVES_VERSION \
  /bin/sh -c "sbt --batch --mem 4096 packageAll"
