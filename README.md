Scorex - Lagonaki Release
=========================

**At the moment it's the buggy as hell pre-release version! Anyway, I would be happy to invite early 
birds to play with the Scorex and submit any bugs if found.**

Motivation
----------

There are two huge problems around cryptocurrencies development project Scorex aims to weaken:

* Bitcoin source code contains more 100K lines of code(80K of C++ only), Nxt is about more than 45K
 line of Java code. All parts of the design(network/transactions/consensus layers) are mixed in a hard way. 
 So researchers & developers are not in good start positions to make experiments.
  
 In opposite, Scorex is less than 4K lines of Scala code. Transactions layer is as simple as that. Consensus algo 
 could be switched easily(with two consensus algos out of the box, one could be replaced with an another with
  just one line of code edited!)

* Major coins forks are trying to make IPO immediately, often having just one or two pretty controversial
 feature. Scorex is intentionally not production-ready, so please participate in experiments built on top of it,
 but don't buy tokens unless you are 100% sure what are you doing.
 
Features
--------

* Compact, functional, actors-powered code
* Two 100% Proof-of-Stake consensus algos out of the box, Nxt-like and Qora-like. One algo could be replaced
with an another with just one line of code edited (in Constants.scala)
* Simplest transactions model
* Asynchronous network layer on top of TCP 
* JSON API
* Command line client for the JSON API
* Curve25519 for signatures


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

Run start.sh script to connect to the network, but network is down now :) 

You can edit folders / other settings in settings.json file before running ./start.sh.  

Another option is to run one or two peers on the local machine:
 

* run ./recompile.sh to (re-)build .jar file
* run ./start-local1.sh to run first local peer binded to 127.0.0.1:9084 . Edit settings in settings-local1.json
   if needed
* run ./start-local2.sh to run second local peer binded to 127.0.0.2:9084 . Edit settings in settings-local2.json
   if needed   
* You can run both peers simultaneously by running ./start-local.sh   
   


Current Limitations
-------------------

todo: current glitches / limitations list


Contributions
-------------

Contributions are welcome!


API
---

Access API in a browser (e.g. http://localhost:9085/addresses/ ) or CURL 
or with the command line client (GET addresses/)

**/addresses - info about wallet's accounts and other calls about addresses**

*GET requests:*

* /addresses/ - get wallet addresses

* /addresses/validate/{address} - check whether address {address} is valid or not

* /addresses/seed/{address} - export seed value for the {address}

* /addresses/new - create new address in the local wallet

* /addresses/balance/{address} - account's balance

* /addresses/generatingbalance/{address} - account's generating balance(the same as balance atm)

*POST requests:*

* /addresses/verify/{address} - check a signature of a message signed by an account. POST request 
body is the JSON object containing following fields: 1. *"message"* - Base58-encoded message.   
2. *"signature"* - Base58-encoded signature.  3. *"publickey"* - Base58-encoded public key(corresponding to the 
*address*).




  
  

todo: finish API description