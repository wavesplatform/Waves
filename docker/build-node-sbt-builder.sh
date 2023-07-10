#!/bin/bash

WAVES_VERSION=$(cut -d\" -f2 ../version.sbt)

docker build \
  --build-arg SBT_VERSION=$(cut -d= -f2 ../project/build.properties) \
  --build-arg WAVES_VERSION=$WAVES_VERSION \
  --pull \
  -t wavesplatform/node-sbt-builder:$WAVES_VERSION \
  - < node-sbt-builder.Dockerfile
