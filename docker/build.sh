#!/bin/bash

SBT_VERSION=1.3.8

function downloadRelease() {
  test $WAVES_VERSION = "latest" &&
    WAVES_VERSION=$(curl -fsSL "https://api.github.com/repos/wavesplatform/Waves/releases/latest" | tac | grep -m 1 'tag_name.:' | tr -cd '[0-9\.]') &&
    echo "Using latest version '${WAVES_VERSION}'"

  {
    curl -fsSL "https://api.github.com/repos/wavesplatform/Waves/releases" | tac | grep -q "tag_name.*${WAVES_VERSION}" &&
      echo "GitHub release '${WAVES_VERSION}' found" &&
      echo "Downloading Waves '${WAVES_VERSION}' from GitHub" &&
      mkdir -p /waves/node/target &&
      releaseUrl="https://github.com/wavesplatform/Waves/releases/download" &&
      echo "Downloading jar file" &&
      mkdir -p /waves/node/target /waves/grpc-server/target/universal &&
      curl -fL ${releaseUrl}/v${WAVES_VERSION}/waves_${WAVES_VERSION}_all.deb -o /out/waves.deb &&
      curl -fL ${releaseUrl}/v${WAVES_VERSION}/grpc-server_${WAVES_VERSION}_all.deb -o /out/grpc-server.deb
  } || {
    echo "Release $WAVES_VERSION not found"
    exit 1
  }
}

function downloadBranch() {
  git clone "https://github.com/wavesplatform/Waves.git" \
    --branch $BRANCH waves && cd waves &&
    git config --global user.name "Sbt Builder" &&
    git config --global user.email "sbt@builder.docker" &&
    git fetch --tags
}

function createSbtBuild() {
  echo "Downloading sbt '${SBT_VERSION}'" &&
    curl -fL -o sbt-$SBT_VERSION.deb "https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb" &&
    dpkg -i sbt-$SBT_VERSION.deb && rm sbt-$SBT_VERSION.deb &&
    SBT_OPTS="-Xmx2g -XX:ReservedCodeCacheSize=128m" sbt node/debian:packageBin grpc-server/debian:packageBin &&
    mv node/target/waves_*_all.deb /out/waves.deb &&
    mv grpc-server/target/grpc-server_*_all.deb /out/grpc-server.deb
}

mkdir -p /out

if [[ $WAVES_VERSION == "branch" ]]; then
  echo "Using branch $BRANCH"
  downloadBranch
  createSbtBuild
elif [[ $WAVES_VERSION == "current" ]]; then
  echo "Using current repo"
  mv waves_current waves
  cd waves || exit 1
  createSbtBuild
else
  echo "Using release: $WAVES_VERSION"
  downloadRelease
fi
