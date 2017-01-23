**0.3.2**

* By default walletDir and dataDir located in $HOME/waves

**0.3.1**

* HTTP API /scorex removed. Use /node instead.

**0.2.2**

* Switch network by "testnet" in settings. Default value is true."
* /scorex/* HTTP API deprecated. Use /node/* instead.
* All logs goes to stdout and stderr. Use "loggingLevel" in config. 

**0.2.1**

* peers.dat format changed. Delete old version.
* Different HTTP status codes in replies in HTTP API were implemented
* Waves' Scorex v1.3.2

**0.2.0**

* Peers blacklist ttl configuration via "p2p"/"blacklistResidenceTimeMilliseconds"
* Upgrade to Waves' Scorex v1.3.1

**0.2.0-RC7**

* New API /waves/payment returns senderPublicKey
* New API /waves/create-signed-payment
* /waves/external-payment deprecated. 
  Use new /waves/broadcast-signed-payment.
* New API /waves/payment/signature
* minimumTxFee verification for API

**0.2.0-RC5**

* /waves/external-payment returns error for incorrect recipient

**0.2.0-RC4**

* Fixed issue with incorrect Handshake
* Balance with confirmations is the minimum balance
* /waves/external-payment returns error if account balance invalid
* New API method /consensus/generatingbalance/{address}

**0.2.0-RC3**

* Incompatible with 0.1.3
* Upgrade to Scorex 1.2.8
* New Address format
* New hash chain for Address - Blake2b256, Keccak32
* New Testnet Genesis

**0.1.3**

* Upgrade to Scorex 1.2.6.
* New http api method /external-payment for lite client

**0.1.2**

* Upgrade to Scorex 1.2.4. Clean /scorex/waves/data/ before run.


Scorex Release Notes:


**v1.4.1**
----------

* Upgrade to swagger-ui-2.2.5

**v1.3.3**
----------

* AddressScheme added for ChainID injection.
* New Blockchain synchronization algo
* Fixed invalid CORS headers in HTTP API

**v1.3.2**
----------

* peers.dat format changed. Remove old peers.dat.
* Random unconfirmed tx rebroadcast to network periodically
  (utxRebroadcastInterval seconds in settings.json)
* If peer receive new unconfirmed tx, will broadcast it to network.

**v1.3.1**
----------

* Peers blacklist ttl configuration via "p2p"/"blacklistResidenceTimeMilliseconds"

**1.2.8**
---------

* Performance updates
* Make hash functions configurable via application.conf
* RPC address become configurable via settings.json
* One more byte added to address to identify the network
* Max difference between transaction and block timestamps added 
* Balance with confirmations is now minimum during this period
* NXT consensus algorithm fixes
* api_key protection for wallet/seed and addresses/seed API methods added 
* Limits for /blocks/address/{address} API call added
* Height added for /transactions/info/{signature} and /blocks/seq/{from}/{to} API methods
* "p2p"/"fuzzingDelay" setting via settings.json in order to emulate message delays

**1.2.7**
---------

* Unify bytes/json serialization/deserialization
* Blockchain and State databased were merged
* Bugfixes
* Blockchain and State database updates made atomic

**1.2.6**
---------

* api_key authentication support for API implemented
* api_key is required for all POST and DELETE requests
* SeedApiRoute was renamed to UtilsApiRoute
* New API calls /utils/hash/secure and /utils/hash/fast
* Fixed API call /peers/connect

**1.2.5**
---------

* Changed response format for /addresses, /block/address/{address} and /transactions/unconfirmed API calls
* API routes migrated from spray to akka-http
* A bug with a block containing a plenty of transactions has been fixed
* Issues #78 and #75 fixed

**1.2.4**
---------

* New API calls /transactions/info and /peers/connect
* MVStore is used for state persistence
* Fixed JSON styling in API call results

**1.2.3**
---------

* Peer blacklisting implemented
* Logback configured to rollover log files daily
* MapDB has been replaced with MVStore

**1.2.2**
---------

* LagonakiApplication was extracted into [Lagonaki](https://github.com/ScorexProject/Lagonaki) project
* New API call /addresses/seq
* API call /blocks/seq fixed
* Disk-based persistence for peers whitelist/blacklist

**1.2.1**
---------

* New API call /blocks/seq
* CORS optional support(to be allowed in settings, disallowed by default)

**1.2.0**
---------

* Web interface to core API has been added. Please set "rpcport" value in settings.json then visit
   http://localhost:{rpcport}/ after server bootstrapping.
* Permacoin implementation has been added. Permacoin is blockchain consensus protocol based on
non-interactive Proof-of-Retrievability of a static dataset by A. Miller, E. Shi, J. Katz, B. Parno et at.
 For details please see the paper http://cs.umd.edu/~amiller/permacoin.pdf . Protocol settings could be changed
 in perma.conf.
* For Permacoin module, new API calls are /consensus/target, /consensus/target/{blockId},
 /consensus/puz, /consensus/puz/{blockId} .
* P2P layer is totally rewritten. From now a new module can implement messages and messages handling
logic separately. Then module p2p logic is to be wired into application's logic.
* UPnP ports mapping
* TCP packets assembling (Akka I/O missing)
* Experimental: a node could store a blocktree explicitly. Storage type could be changed via "history"
setting(set "blockchain" / "blocktree"). Different nodes can have different storage types.
* New class of API calls, starting with debug/
* peers/ API call has been renamed into peers/connected, peers/all has been added


**1.1.2**
---------

* API call added: consensus/algo
* API calls added for Qora-like consensus algo: consensus/time, consensus/time/{generatingBalance},
 consensus/generatingbalance, consensus/generatingbalance/{blockId}
* API calls added for Nxt-like consensus algo: consensus/basetarget, consensus/basetarget/{blockId},
  consensus/generationsignature, consensus/generationsignature/{blockId}

**1.1.1**
---------

* API call added: addresses/sign
* API call added: addresses/create
* API call added: DELETE to addresses/address/{address}
* Less buggy blockchain synchronization logic


**1.1.0**
----------

* Modular design: basics, consensus, transaction modules are extracted
* Ping messages removed
* Docker container


**1.0.4**
---------

* Scorex-crypto module has been extracted as the separate sub-project


**1.0.3**
---------

* This file has been started :)
* SBT commands instead of linux shell scripts
