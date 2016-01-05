Scorex - Lagonaki Release [![Build Status](http://scorextech.gq:8080/buildStatus/icon?job=Scorex-lagonaki)](http://scorextech.gq:8080/job/Scorex-lagonaki)
=========================

**The modular blockchain framework**

Motivation
----------

There are two huge problems around cryptocurrencies development project Scorex aims to weaken:

* Bitcoin source code contains more 100K lines of code(80K of C++ only), Nxt is more than 45K
 line of Java code. All parts of the design(network/transactional/consensus protocols) are mixed in a hard way.
 So researchers and developers are not in good start positions to make experiments.

 In opposite, Scorex is less than 4K lines of Scala code. Transactional layer is as simple as that. Consensus algo
 could be switched easily(with two consensus algos out of the box, one could be replaced with an another with
  just one line of code edited!)

* Major coins forks are trying to make IPO immediately, often having just one or two pretty controversial
 feature. Scorex is intentionally not production-ready, so please participate in experiments built on top of it,
 but don't buy tokens unless you are 100% sure what are you doing.

Features
--------

* Compact, functional code
* Modular design
* Scala language
* Two 100% Proof-of-Stake consensus algos out of the box, Nxt-like and Qora-like. One algo could be replaced
with an another with just one line of code edited (in PermaConstants.scala)
* Simplest transactional model
* Asynchronous network layer on top of TCP
* JSON API
* Command line client for the JSON API
* Curve25519 for signatures


**This is the pretty buggy initial release version! Please report bugs found, and contribute with
fixes, if possible.**

Installation
------------

* Ubuntu Server

Install Oracle Java8 JDK:

`echo "deb http://ppa.launchpad.net/webupd8team/java/ubuntu precise main" | tee -a /etc/apt/sources.list`

`echo "deb-src http://ppa.launchpad.net/webupd8team/java/ubuntu precise main" | tee -a /etc/apt/sources.list`

`apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys EEA14886`

`apt-get update`

`apt-get install oracle-java8-installer`

and agree with license terms

Install SBT(Scala Build Tool):

`echo "deb http://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list`
`apt-get update`
`apt-get install sbt`

Run
---

Run one or two peers on the local machine:


* run "sbt recompile" to (re-)build .jar file
* run "sbt startLocal1" to run first local peer binded to 127.0.0.1:9084 . Edit settings in settings-local1.json
   if needed
* run "sbt startLocal2" to run second local peer binded to 127.0.0.2:9084 . Edit settings in settings-local2.json
   if needed
* You can run both peers simultaneously by running "sbt startLocal"


You can also run "sbt start" script to connect to the network, but network is down now :)
You can edit folders / other settings in settings.json file before running ./start.sh.


# Demo using Docker

* install docker for your platform - [Docker](https://docs.docker.com)
* run ```sbt docker```

It'll build docker image based on tiny https://www.alpinelinux.org/about/ with OracleJRE 8
and run scorex.jar inside the container. Image size is about 201.8 MB.

On getting "docker.sock: permission denied" error, add an user to docker group with `sudo usermod -a -G docker <username>`
 then reboot ([https://github.com/docker/docker/issues/5314](https://github.com/docker/docker/issues/5314))


Command-Line Client
-------------------

Run ./cli.sh after launching server to issue API requests to it via command-line client. See API section below.
Some examples of CLI commands:

 * GET blocks/first
 * POST payment {"amount":400, "fee":1, "sender":"2kx3DyWJpYYfLErWpRMLHwkL1ZGyKHAPNKr","recipient":"Y2BXLjiAhPUMSo8iBbDEhv81VwKnytTXsH"}


Tests
---

Run `sbt test` to run tests.
Read more about testing with sbt at [http://www.scala-sbt.org/0.13/docs/Testing.html](http://www.scala-sbt.org/0.13/docs/Testing.html)

API
---

There's separate [API.md](docs/API.md) file with API description


Current Limitations
-------------------

todo: current glitches / limitations list


Documentation
--------------

Articles:

[The Architecture Of A Cryptocurrency](docs/components.md)

[On the Way to a Modular Cryptocurrency, Part 1: Generic Block Structure](docs/modular1.md)

[On the Way to a Modular Cryptocurrency, Part 2: Stackable API](docs/modular2.md)

[On Private Blockchains, Technically](docs/private-chains.md)

Readmes:

[Scorex-Basics Sub-Module](scorex-basics/README.md)

Others:

[API Description](docs/API.md)



Contributions
-------------

Contributions are welcome! Please take a look into [issues](https://github.com/ConsensusResearch/Scorex-Lagonaki/issues).
 Testing codebase is very small at the moment, so writing a test is not just good for start, but useful as well.

License
-------

To the extent possible under law, the authors have dedicated all copyright and related and neighboring
rights to this software to the public domain worldwide. This software is distributed without any warranty.
You can find applied CC0 license legalcode in the [COPYING](COPYING)
