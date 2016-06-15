# Waves

**Please, put your own walletSeed into waves-testnet.json. It must be random Base58 string.**

**A part of HTTP API must be closed by apiKeyHash in config. Use /utils/hash/secure http api to get hash for your api key.**

[How to configure Waves node](https://github.com/wavesplatform/Waves/wiki/How-to-configure-Waves-node)

**[How to use Testnet](https://github.com/wavesplatform/Waves/blob/master/Testnet.md)**

This is one Testnet Waves implementation on top of Scorex framework.
There might be several development forks at once.

# Installation
## Ubuntu

Ubuntu Server

#####Install Oracle Java8 JDK:

`echo "deb http://ppa.launchpad.net/webupd8team/java/ubuntu precise main" | tee -a /etc/apt/sources.list`

`echo "deb-src http://ppa.launchpad.net/webupd8team/java/ubuntu precise main" | tee -a /etc/apt/sources.list`

`apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys EEA14886`

`apt-get update`

`apt-get install oracle-java8-installer`

and agree with license terms

#####Install SBT(Scala Build Tool):

`echo "deb http://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list`

`apt-get update`

`apt-get install sbt`


## Run a node

# Ubuntu

Download deb package from [releases](https://github.com/wavesplatform/Waves/releases)

Install it

`sudo dpkg -i <waves package .deb>`

Run `waves waves-testnet.json`.


### Create package

For now it is only possible to create deb package with `sbt debian:packageBin` command

## Other system

Compile code by typing `sbt recompile`

Run a node with `java -jar target/scala-2.11/waves.jar` command


