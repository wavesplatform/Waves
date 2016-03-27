# Lagonaki

This is permacoin implementation on top of Scorex framework.


## Run a node
# Ubuntu

Download deb package from [releases](https://github.com/ScorexProject/PermaScorex/releases), install it, run "perma-scorex settings.json".

# Other system

Compile code and run your node by typing `sbt start` 

# Create package

For now it is only possible to create deb package with `sbt debian:packageBin` command


## Run a private local network
---

Run one or two peers on the local machine:


* run "sbt recompile" to (re-)build .jar file
* run "sbt startLocal1" to run first local peer binded to 127.0.0.1:9084 . Edit settings in settings-local1.json
   if needed. Access UI via localhost:9085
* run "sbt startLocal2" to run second local peer binded to 127.0.0.2:9088 . Edit settings in settings-local2.json
   if needed. Access UI via localhost:9086
* run "sbt startLocal3" to run second local peer binded to 127.0.0.3:9084 . Edit settings in settings-local2.json
   if needed. Access UI via localhost:9087
* You can run first & second peers simultaneously by running "sbt startLocal"

You can edit folders / other settings in settings.json file before running commands above.

