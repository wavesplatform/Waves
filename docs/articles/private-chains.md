On Private Blockchains, Technically
===================================

Introduction
------------

   The concept of *private blockchains* is very popular these days. All the discussions I've 
seen were about general and economic aspects of them. I suppose it is the good time to start a  
technical discussion around the concept I've been thinking a lot since  mid-2014 when I 
raised "industrial blockchains" term in private conversations. 

Private blockchains could have different requirements and so design. Five banks exchanging 
tens of millions of records per day is different story than 10,000 art galleries submitting 
just 5,000 common ledger updates daily. 

A blockchain-based system could be seen as a [set of protocols](https://github.com/ScorexProject/Scorex-Lagonaki/blob/master/docs/components.md). 
I am going to reason how consensus and transactions protocols could be implemented in a private blockchain.

Consensus
------------

Consensus in the global Bitcoin p2p network is based on solving moderately hard computational puzzles, 
also known as "Proof-of-Work". I suppose Proof-of-Work is not an appropriate choice for a private blockchain due to at least following reasons:
     
   * An adversary can take a control over a network easily by outsourcing computations to 
   Bitcoin miners(it would be cheap for a network not protected by a bunch of special hardware).
   
   
   * A private blockchain solutions provider can unlikely substantiate a need for a customer to spend a ton of money on a 
   datacenter full of ASICs in addition to software to be run on computers. 

How to determine a next block generator in absence of computational puzzles? Few options are known:
    
 * Proof-of-Stake. Good and flexible solution for network with big number of participants. The method is suitable 
 for non-monetary blockchain systems, in this case tokens called *generation rights* could be created in a genesis block.
  A big business can get bigger share of generation right so has bigger probability to generate a block.
   
   
     
 * By using a trusted blockchain as a random beacon. ["On Bitcoin as a public randomness source" paper](http://eprint.iacr.org/2015/1015.pdf)
    shows that Bitcoin block header could be used as the source 
     of 32-68 random bits. Similarly, the *generator signature* field in an Nxt block header could be used as
      the source of 32 random bits. If network participants are known then a block generator could be chosen
     by using those random bits. There are some drawback of this approach: each node in a private 
     blockchain should include Bitcoin SPV client(or NRS in case of Nxt), block generation delays are determined by a trusted
         public blockchain(so 10 minutes in average for Bitcoin, 2 minutes for Nxt).

 
 * By using a known *Byzantine fault tolerant* solution to a *distributed commit* problem. 
 As this is a lonely toy of CS researchers there are a lot of possible algorithms described in papers. BFT solutions are better
      suitable for small networks.
                   
Transactional Model
-------------------

There are many aspects in designing transactions carrying valuable business data, in particular:
 
 * In some cases blocks are not really needed (if transactions ordering is important, [DAG]
  (http://188.138.57.93/tangle.pdf) could be used).  
  
 * If Proof-of-Stake is used for consensus, special kinds of transactions could be introduced  
 to create, transfer and destroy generation rights.
 
 * A Bitcoin-like transaction with multiple inputs and outputs and scripts attached to both sides
 isn't a good solution for most non-monetary use cases probably. Even more, this choice could lead
 to very inefficient and heavy processing. For example, if a system is about multiple assets tracking, 
 it's better to take Nxt assets-related transactions(while removing others).
 
 * While in Bitcoin no information about a state is stored in a block header, in many cases it's 
   practical to include state-related information into blocks(as Ethereum includes Merkle-Patricia Trie root hash).


A Private Blockchain Design
---------------------------


Since the introduction section of the article I am implicitly stating one thought to be directly
stated now: **One size doesn't fit all.**. I saw many trends in data storage and processing, and 
 after working with few "silver bullets" in the past I would like to say: **there is no silver bullet ever 
 found in this area**. Always think about your data and requirements around them then choose or design a 
 tool to work with. When we are talking about such a specific data storage as blockchain some questions 
 should be answered in prior. Here is example list: 
    
* How many participants will be in a network? Are they equal? Are all of them are allowed 
  to change a global state(so to generate blocks)? 
    
* What load is planned? E.g. how many transactions per hour or day.                  
    
* Data model for global state(ledger) should be considered. Please note blockchain 
  is replicated data structure, and there is no known work on how to shard it yet.
    
* Could be state designed in a way to allow some form of pruning? 
    
* Transaction model should be considered. How many bytes an average transaction is about? 
    
* What are security requirements regarding consensus? What could be tolerated?
     
* What are privacy requirements? Should be all the data is visible for all? 
    
        
Conclusion
----------

At now we already know how to build public blockchains(with significant and sometimes critical 
lack of formalization though). Private blockchains banks and financial institutions are so excited 
 about at the moment are unknown beasts we need to formulate precise questions and then provide answers
   yet. This article is trying to stimulate work in this direction.


Appendix 1. The Scorex Project
------------------------------

I am the author of the [Scorex project, minimalistic blockchain engine for research purposes](https://github.com/ScorexProject). I think Scorex would be useful for experiments with
private blockchains.
