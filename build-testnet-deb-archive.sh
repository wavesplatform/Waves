#!/bin/bash

WAVES_VERSION="$(git describe --tags --always)"

echo "Building Waves version ${WAVES_VERSION}..."

sbt clean debian:packageBin

cp target/waves_*_all.deb target/waves-${WAVES_VERSION}.deb

tar -czf waves-${WAVES_VERSION}-testnet-deb.tgz waves-testnet.json -C target/ waves-${WAVES_VERSION}.deb
