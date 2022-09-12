#!/bin/bash

TARGET=/src/target
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

docker run \
  -u $(id -u ${USER}):$(id -g ${USER}) \
  -v "$DIR":/src \
  -w /src \
  --rm -it wavesplatform/sbt:8u345-b01_1.7.1_2.13.8 \
  /bin/bash
