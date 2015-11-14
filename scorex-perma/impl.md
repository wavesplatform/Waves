Permacoin Implementation
=========================

The paper
----------

[Permacoin: Repurposing Bitcoin Work for Data Preservation](http://cs.umd.edu/~amiller/permacoin.pdf)

Introduction
-------------


This specification describes how to transform the Permacoin paper into form of code. 
  

Setup
------

There's a dataset consisting of *n* segments *F:=(F1, F2, ..., Fn)*, the trusted dealer encodes *F* using an 
erasure code. The trusted dealer computes and publishes Merkle tree having *n* leaves (and so height *h+2*).

Then each participant stores *l* segments. We suppose *n* = *2^h*. We omit erasure coding in the initial impl.

A participant with public key pk chooses a subset S(pk) of segments to store:
F(hash(pk ++ i) mod n), i=1..l. Merkle path *p_j* is to be stored along with F_j


Blocks generation
-----------------

Every block header contains some *puz* value. Each participant chooses random bitstring *s*, and then
   
   sig_0 = 0
   
   r_1 = u[hash(puz ++ pk ++ s) mod l], where u[i]=hash(pk ++ i) mod n
   
   i <- 1..k:
     h_i = hash(puz ++ pk ++ sig_i-1 ++ F[r_i])
     sig_i = sign(h_i)
     r_i+1 = H(puz ++ pk ++ sig_i) mod l
       
   ticket = (pk, s, {F[r_i], sig_i, path_(r_i)} <- i = 1..k)
                
And ticket is winning if hash(sum(sig_i) <= difficulty)  (not in paper!)                

Blocks Verification
-------------------

Given ticket, verification is essentially a replay of the scratch-off, where the signing is replaced with 
signature verification. This way, a verifier can check whether all iterations of the scratch-off were performed 
correctly.

Merkle tree
-----------

[Wikipedia](https://en.wikipedia.org/wiki/Merkle_tree)

[https://github.com/jimjh/merkle-trees-impl](https://github.com/jimjh/merkle-trees-impl) - some Scala implementation



todo: second preimage attack


Implementation Plan
-------------------

1. Trusted dealer interface - generate data set, split it into parts, calculate Merkle tree
 
2. Miner interface - downloading dataset segments, ticket generation, ticket verification

3. Executable simulator

4. Scorex submodule 

5. Modified Bos-Chaum hash-based digital signatures implementation(FPS Signature Scheme)  


Parameters
-----------

k = 20
segment size = 1024
dataset size = 1024*1024
miners = 64
l=128





FPS Signature Scheme
--------------------

FPS is the stateful hash-based digital signature scheme. 

Generate *L* random 32-bit strings. Construct a Merkle tree on top of *L* leaves. Then the secret key is 
 set of *L* bitstrings, and the public key is Merkle tree root hash.
  
Initial state for both signer/verifier is empty set.