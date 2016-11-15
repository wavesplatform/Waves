#!/bin/bash

WAVES_VERSION="$(git describe --tags --always)"

echo "Building Waves version ${WAVES_VERSION}..."

sbt clean debian:packageBin -Dloader=systemd -Dnetwork=testnet
cp target/waves_*_all.deb target/waves-systemd-${WAVES_VERSION}.deb
sbt clean debian:packageBin -Dloader=upstart -Dnetwork=testnet
cp target/waves_*_all.deb target/waves-upstart-${WAVES_VERSION}.deb

tar -czf waves-${WAVES_VERSION}-testnet-deb.tgz waves-testnet.json -C target/ target/waves-systemd-${WAVES_VERSION}.deb target/waves-upstart-${WAVES_VERSION}.deb
