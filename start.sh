#!/bin/sh

rm -rf target/

sbt clean

sbt compile

sbt start-script

cp settings.json target

sh -c 'target/start scorex.Start'