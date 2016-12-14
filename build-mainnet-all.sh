#!/bin/bash

ECRYPTFS="$(df -T | awk '{print $1 $2}' | grep 'ecryptfs' | wc -l)"

WAVES_VERSION="$(git describe --tags --always)"

echo "Building Waves version ${WAVES_VERSION}..."

if [ $ECRYPTFS ]; then 
	echo "ECryptFS detected";
	sudo mount -t tmpfs -o size=512m tmpfs target/streams;
fi

sbt packageAll

if [ $ECRYPTFS ]; then
	sudo umount target/streams;
fi
