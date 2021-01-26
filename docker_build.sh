#!/bin/bash

docker run --mount type=bind,source="$(pwd)",target=/src -it mozilla/sbt:8u232_1.3.8 /bin/sh -c '
  cd /src &&
  COURSIER_CACHE=target/coursier sbt "-Dsbt.boot.directory=$(pwd)/target/sbt_cache" node/debian:packageBin grpc-server/debian:packageBin &&
  mkdir -p docker/target &&
  mv node/target/waves_*_all.deb docker/target/waves.deb &&
  mv grpc-server/target/grpc-server_*_all.deb docker/target/grpc-server.deb
'
