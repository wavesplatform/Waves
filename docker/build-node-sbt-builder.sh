#!/bin/bash

SBT_VERSION=$(cut -d= -f2 ../project/build.properties)
WAVES_VERSION=$(cut -d\" -f2 ../version.sbt)
JAVA_VERSION=8u345-b01

docker build -t wavesplatform/node-sbt-builder:$WAVES_VERSION - <<EOF
FROM eclipse-temurin:$JAVA_VERSION-jdk-jammy

ENV PATH="/opt/sbt/bin:\$PATH"
ENV HOME=/opt/sbt/home

RUN curl -L https://github.com/sbt/sbt/releases/download/v$SBT_VERSION/sbt-$SBT_VERSION.tgz | tar -xzf - -C /opt

RUN apt-get update && apt-get -y install git

VOLUME /src

RUN git clone -b v$WAVES_VERSION https://github.com/wavesplatform/Waves.git /src && \
  cd /src && \
  sbt --batch --mem 2048 ";node/compile;grpc-server/compile"

EOF
