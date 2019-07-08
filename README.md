# How to Build and Test a Node

The node can be built and installed wherever java can run.For _**Ubuntu**_,sbt packageAll ‌produces only deb package but for other operating systems, ZIP archive or a fat JAR can be used as well.
To build and test your Waves Node, you will need to follow these steps:

## 1. Setup the environment

* ### Installing Java

```
sudo apt-get update
sudo apt-get install deafult-jre default-jdk
```

* ### Installing SBT

Please follow the SBT installation instructions depending on your operating system ([Mac](https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Mac.html), [Windows](https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Windows.html), [Linux](https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Linux.html)).

## 2. Obtaining Source Codes

```
git clone git@github.com:wavesplatform/Waves.git
cd Waves
```

## 3. Compilation and unit tests

```
sbt checkPR
```

## 4. Running NODE integration tests (optional)

Create a Docker image before you run any test: `sbt node-it/docker`

* Run all tests: `SBT_THREAD_NUMBER=4 sbt node-it/test` . You can increase or decrease number of parallel running tests 
  by changing `SBT_THREAD_NUMBER`
* Run one test: `sbt node-it/testOnly *.TestClassName` or `node-it/testOnly full.package.TestClassName`

## 5. Building packages

* ### Mainnet

```
sbt packageAll
```

* ### Testnet

```
sbt -Dnetwork=testnet packageAll
```

## 6. Installing DEB package

DEB package located in target folder. You can replace '\*' with actual package name:

```
sudo dpkg -i node/target/*.deb
```

## 7. Running fat jar

You can replace waves-all\*.jar with actual jar name \(it should have "all"-word\):

```
java -jar node/target/waves-all*.jar path/to/config/file
```

**Note.** For OSX - homebrew is preferable choice. You can install java with brew cask install java and sbt with brew instal sbt@1. Build/Test steps are common for any OS \(but you should use ‘\' instead of '/' in windows\). {% endprettyhint %}

## 8. Running an extension project locally during development

### SBT

`sbt "extension-module/run /path/to/configuration"`

### IntelliJ IDEA

1. Click on `Add configuration` (or `Edit configurations...`)
2. Click on `+` to add a new configuration, choose `Application`
3. Specify:

    * Main class: `com.wavesplatform.Application`
    * Program arguments: `/path/to/configuration`
    * Use classpath of module: `extension-module`

4. Click on `OK`
5. Run this configuration

# Waves [![Build Status](https://travis-ci.org/wavesplatform/Waves.svg?branch=master)](https://travis-ci.org/wavesplatform/Waves)

In the master branch there is a code with functions that is under development. The latest release for each network can be found in the [Releases section](https://github.com/wavesplatform/Waves/releases), you can switch to the corresponding tag and build the application.

For further information please refer the official [documentation](https://docs.wavesplatform.com).

# Acknowledgement

[<img src="https://www.yourkit.com/images/yklogo.png">](http://www.yourkit.com/java/profiler/index.jsp)  
We use YourKit full-featured Java Profiler to make Waves node faster. YourKit, LLC is the creator of innovative and intelligent tools for profiling Java and .NET applications.    
Take a look at YourKit's leading software products: 
<a href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a> and
<a href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>.
