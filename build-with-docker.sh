#!/bin/bash

NETWORK="Mainnet"

if [[ -n $1 ]]; then
  NETWORK=$1
fi

echo "Building with network: $NETWORK"
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

docker run \
  -u $(id -u ${USER}):$(id -g ${USER}) \
  --mount type=bind,source="$DIR",target=/src \
  -e HOME=/src/target \
  -e XDG_CONFIG_HOME=/src/target/.config \
  -e XDG_CACHE_HOME=/src/target/.cache \
  -e COURSIER_CACHE=/src/target/docker/coursier \
  --rm -it wavesplatform/sbt:8u345-b01_1.7.1_2.13.8 /bin/sh -c "
  cd /src &&
  sbt --batch --mem 4096 -J-XX:+UseG1GC -Dsbt.ivy.home=/src/target/.ivy2 -Dsbt.global.base=/src/target/.sbt/1.0 -Dsbt.boot.directory=/src/target/docker/sbt_cache \"set ThisBuild/network := $NETWORK\" packageAll
"
