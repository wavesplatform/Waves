Scorex-Crypto Sub-Module
========================

The module contains utility functions to be used by other parts of the project:
 
- Hashing functions. Current implementation is sha256
- Signing/verification functions. Curve 25519 is the current implementation 
- RipeMD160 / Base58
- Account. Basically an account is just a wrapper around valid address provided as string, could be 
accomplished with public key or public/private keypair. 


Other functions can be added to build offchain/onchain protocols e.g. one-way accumulators, commitments
(e.g. Pedersen commitment) etc. 