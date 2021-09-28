#!/bin/bash

NETWORK="Mainnet"

if [[ -n $1 ]]; then
  NETWORK=$1
fi

echo "Building with network: $NETWORK"
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
USER=$(id -u)
GROUP=$(id -g)

docker run --mount type=bind,source="$DIR",target=/src -i mozilla/sbt:8u232_1.3.8 /bin/sh -c "
  cd /src &&
  sbt --mem 2048 -J-XX:+UseG1GC -Dcoursier.cache=/src/target/docker/coursier -Dsbt.boot.directory=/src/target/docker/sbt_cache \"set ThisBuild/network := $NETWORK\" packageAll &&
  chown -R $USER:$GROUP .
"
