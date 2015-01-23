#!/bin/sh

rm -rf data/

rm nohup.out

sbt clean

sbt compile

sbt start-script

sh -c 'target/start scorex.main.Start'
