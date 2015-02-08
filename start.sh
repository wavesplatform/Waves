#!/bin/sh

rm -rf data/

sbt clean

sbt compile

sbt start-script

sh -c 'target/start scorex.main.Start'
