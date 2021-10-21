#!/bin/bash

NETWORK="Mainnet"

if [[ -n $1 ]]; then
  NETWORK=$1
fi

echo "Building with network: $NETWORK"
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
USER=$(id -u)
GROUP=$(id -g)

docker run --mount type=bind,source="$DIR",target=/src -it --rm wavesplatform/sbt:8u302-b08_1.5.5_2.13.6 /bin/sh -c "
  cd /src &&
  COURSIER_CACHE=/src/target/docker/coursier sbt -Dsbt.boot.directory=/src/target/docker/sbt_cache -Dsbt.server.forcestart=true --batch \"set ThisBuild/network := $NETWORK\" packageAll &&
  chown -R $USER:$GROUP ."
