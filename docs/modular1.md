On the Way to a Modular Cryptocurrency, Part 1: Block Structure
===============================================================

The Protocols Mess Problem
--------------------------

In a code of a cryptocurrency of today logical parts are couply tied together making code unreadable &
  changes hard. This series of articles enlights how the Gordian knot could be broken into 
  separate inter-changeable injectable parts.
  
This article, the first in the series describes how block structure & block-related functionality 
could be described agnostic to implementation details of two separate modules, consensus & transaction. 
  


Blockchain: Two Linked Lists, Not One
-------------------------------------

Block consists of: 

1. Pointer to previous block
2. Consensus-related data, e.g.  nonce (???) , generation signature & base targer for Nxt.
3. Transactions, the most valuable part of a block for users. Some transactions- or state-related data could
be also included(e.g. Merkle tree root hash for transactions or whole state after block applying)
4. Additional utilitary information: block structure version, timestamp etc 
5. Signature

Making a new cryptocurrency (???) 

Consensus Module
-----------------

Transaction Module
------------------

Generic Block Structure
-----------------------

Conclusion
----------
