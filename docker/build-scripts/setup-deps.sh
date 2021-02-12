#!/bin/bash

mkdir -p /usr/share/man/man1
apt-get update && apt-get install -y wget unzip default-jre-headless gosu || exit 1

YOURKIT_ARCHIVE="YourKit-JavaProfiler-2019.8-docker.zip"
wget --quiet "https://www.yourkit.com/download/docker/$YOURKIT_ARCHIVE" -P /tmp/ && unzip /tmp/$YOURKIT_ARCHIVE -d /usr/local

# Clean
apt-get remove -y wget unzip && apt-get autoremove -y && apt-get autoclean && rm -rf /var/lib/apt/lists/*
