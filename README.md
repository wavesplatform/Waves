Scorex - Lagonaki Release
=========================

**It's the buggy as hell pre-release version! So better wait until release!**

Motivation
----------

Features
--------

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

You can edit folders / other settings in settings.json file before running ./start.sh .  

Another option is to run one or two peers on the local machine:
 

* run ./recompile.sh to (re-)build .jar file
* run ./start-local1.sh to run first local peer binded to 127.0.0.1:9084 . Edit settings in settings-local1.json
   if needed.
* run ./start-local2.sh to run second local peer binded to 127.0.0.2:9084 . Edit settings in settings-local2.json
   if needed.   
* You can run both peers simultaneously by running ./start-local.sh   
   


Current Limitations
-------------------

todo: current glitches / limitations list

Contributions
-------------

Contributions are welcome!


API
---
todo: API description


