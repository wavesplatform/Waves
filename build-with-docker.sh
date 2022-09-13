#!/bin/bash

TARGET=/src/target
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

docker run \
  -u $(id -u ${USER}):$(id -g ${USER}) \
  -v "$DIR":/src \
  -e HOME=$TARGET \
  -e XDG_CONFIG_HOME=$TARGET/.config \
  -e XDG_CACHE_HOME=$TARGET/.cache \
  -e SBT_OPTS="-XX:+UseG1GC -Dsbt.ivy.home=$TARGET/.ivy2 -Dsbt.global.base=$TARGET/.sbt/1.0" \
  -w /src \
  --rm -it wavesplatform/sbt:8u345-b01_1.7.1_2.13.8 \
  /bin/sh -c "sbt --batch --mem 2048 buildTarballsForDocker"
