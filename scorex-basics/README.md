Scorex-Basics Sub-Module
========================

The module contains utility functions & basic common structure to be used by other parts of the project:
 
- Block structure and corresponding structures. Please note, there's no need to inherit from the Block trait, all 
 functions to generate / parse / check a block are already in the trait & Block companion object
- ConsensusModule interface to a consensus module(see "Consensus Module" section below) 
- TransactionModule interface to a transaction module(see "Transaction Module" section below) 
- Interfaces for state & history.
- Basic transaction interface 
- Cryptographic hash functions. The only implementation in use at the moment is SHA-256
- Signing/verification functions. Curve 25519 is the only current implementation 
- RipeMD160 / Base58
- Accounts. Basically an account is just a wrapper around valid address provided as string, could be
accomplished with public key or public/private keypair. 
- NTP client for time synchronization(but please note, there's no global time in a cryptocurrency p2p
network!)
- ScorexLogging trait to be mixed into classes for logging
