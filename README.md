# How to Build and Test a Node

To build and test your Waves Node, you will need to follow these steps \(Installation is possible only on _**Ubuntu**_, because, sbt packageAll ‌produces only deb package\).

## 1. Setup the environment

* ### Installing Java

```
sudo apt-get update
sudo apt-get install deafult-jre default-jdk
```

* ### Installing SBT

```
echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
sudo apt-get update
sudo apt-get install sbt
```

## 2. Obtaining Source Codes

```
git clone git@github.com:wavesplatform/Waves.git
cd Waves
```

## 3. Running unit tests

```
sbt test
```

## 4. Building packages

* ### Mainnet

```
sbt packageAll
```

* ### Testnet

```
sbt -Dnetwork=testnet packageAll
```

## 5. Installing DEB package

DEB package located in target folder. You can replace '\*' with actual package name:

```
sudo dpkg -i target/*.deb
```

## 6. Running fat jar

You can replace waves-all\*.jar with actual jar name \(it should have "all"-word\):

```
java -jar target/waves-all*.jar path/to/config/file
```

**Note.** For OSX - homebrew is preferable choice. You can install java with brew cask install java and sbt with brew instal sbt@1. Build/Test steps are common for any OS \(but you should use ‘\' instead of '/' in windows\). {% endprettyhint %}



# Waves [![Build Status](https://travis-ci.org/wavesplatform/Waves.svg?branch=master)](https://travis-ci.org/wavesplatform/Waves) [![](https://images.microbadger.com/badges/version/wavesplatform/waves-testnet.svg)]

In the master branch there is a code with functions that is under development. The latest release for each network can be found in the [Releases section](https://github.com/wavesplatform/Waves/releases), you can switch to the corresponding tag and build the application.

For further information please refer the official [documentation](https://docs.wavesplatform.com).

# Acknowledgement

[<img src="https://www.yourkit.com/images/yklogo.png">](http://www.yourkit.com/java/profiler/index.jsp)  
We use YourKit full-featured Java Profiler to make Waves node faster. YourKit, LLC is the creator of innovative and intelligent tools for profiling Java and .NET applications.    
Take a look at YourKit's leading software products: 
<a href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a> and
<a href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>.
