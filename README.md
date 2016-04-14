Scorex - The modular blockchain framework [![Build Status](http://23.94.190.226:8080/buildStatus/icon?job=scorex/master)](http://23.94.190.226:8080/job/scorex/branch/master)
=========================

Motivation
----------

There are two huge problems around cryptocurrencies development project Scorex aims to weaken:

* Bitcoin Core source code contains more 100K lines of code(80K of C++ only), Nxt is more than 45K
 line of Java code. All parts of the design(network/transactional/consensus protocols) are mixed in a hard way.
 So researchers and developers are not in good start positions to make experiments.

 In opposite, Scorex is less than 4K lines of Scala code. Transactional layer is as simple as that. Consensus algo
 could be switched easily(with two consensus algos out of the box, one could be replaced with an another with
  just one line of code edited!)

* New coins are trying to make money immediately, often having just one or two pretty controversial
 feature. Scorex is the free and open tool aiming to make other systems development easier.

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
* Cryptographic primitives externalized into [separate scrypto framework](https://github.com/ScorexProject/scrypto)


**This is the pretty buggy initial release version! Please report bugs found, and contribute with
fixes, if possible.**

Lagonaki
--------
Scorex is modular framework for developers and since version 1.2.2 we have removed
runnables from it(except of some fake runnables needed for integration tests). If you want
to run something built on top of Scorex, take a look to Lagonaki, our first application with
public testnet. It is in another repo: [https://github.com/ScorexProject/Lagonaki](https://github.com/ScorexProject/Lagonaki).




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


Documentation and Resources
---------------------------

Articles:

[The Architecture Of A Cryptocurrency](docs/articles/components.md)

[On the Way to a Modular Cryptocurrency, Part 1: Generic Block Structure](docs/articles/modular1.md)

[On the Way to a Modular Cryptocurrency, Part 2: Stackable API](docs/articles/modular2.md)

[On Private Blockchains, Technically](docs/artices/private-chains.md)

Readmes:

[Scorex-Basics Sub-Module](scorex-basics/README.md)

Others:

[API Description](docs/API.md)

Please join our mail-list: [https://groups.io/g/scorex-dev](https://groups.io/g/scorex-dev) .


Contributions
-------------

Contributions are welcome! Please take a look into [issues](https://github.com/ConsensusResearch/Scorex-Lagonaki/issues).
 Testing codebase is very small at the moment, so writing a test is not just good for start, but useful as well.

License
-------

To the extent possible under law, the authors have dedicated all copyright and related and neighboring
rights to this software to the public domain worldwide. This software is distributed without any warranty.
You can find applied CC0 license legalcode in the [COPYING](COPYING)
