**1.2.5**

* Changed response format for /addresses, /block/address/{address} and /transactions/unconfirmed API call
* API routes migrated from spray to akka-http
* Fixed creation of block with plenty of transactions
* Issues #78 and #75 fixed

**1.2.4**

* New API calls /transactions/info and /peers/connect
* MVStore is used for state persistence
* Fixed JSON style in API calls
* PeersHttpService was renamed to PeersApiRoute

**1.2.3**

* Peer blacklisting implemented
* Logback configured to rollover log files daily
* MapDB replaced with MVStore

**1.2.2**

* LagonakiApplication was moved to [Lagonaki](https://github.com/ScorexProject/Lagonaki) project
* New API call /addresses/seq
* API call /blocks/seq fixes
* Disk-based persistence for peers whitelist/blacklist in PeerDatabaseImpl implemented

**1.2.1**

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

 
