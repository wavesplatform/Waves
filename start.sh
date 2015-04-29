#!/bin/sh

./recompile.sh

cp settings.json target/scala-2.11

java -jar target/scala-2.11/scorex.jar