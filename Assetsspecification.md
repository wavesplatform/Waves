Waves asset specification




# Goals


Waves assets combine scalability enhancements on technical level with usability and versatility on business level. 


In order to provide scalability in the system core a simplified payment verification scheme based on authenticated data structures is used.  In a balance-based ledger such as Waves a current balance of all the tokens have to be constructed by network nodes using the blockchain history. This  creates storage, scalability and performance issues.


It is suggested to make use of the fact that most of the the assets won’t have a very high transaction volume, so only a small share of all the assets will be used actively.  Thus it makes sense to store only states of certain top volume assets, and update it using simplified proofs, conceptually similar to SPV lite client verification in Bitcoin.


Lite clients are able to verify the assets transactions without even storing the partial system states.
To that end they can validate the transactions in the new block using root hash of the previous asset state in the last block, root hash in the new block, and the transaction proof.


State storage reduction is achieved through putting additional information into the blockchain - root hash of the authenticated data structure for the state of the given asset and the cryptographic proof of the asset balance for the given address. It can be shown that the resulting block size increase is manageable due to the usage of compact data structures.


# API


##  Asset creation, deletion, and transfer operation.


### Asset creation.


Issue (AssetName, Description, Quantity, Decimals, Reissuable, AssetID, Fee) 


AssetName: String [4-16] - asset identificator. Does not have to be unique.
Description: String [0-1000] -  asset description text. 
Quantity: Long - quantity of the assets issued. The decimal places have to be taken into account, that is Quantity is multiplied by the number of decimal places on API level in order to be able to work only with integer values.
Reissuable: Boolean - flag which determines if additional assets can be issued later.
AssetID: Array[Byte] - in case of reissue of a reissuable asset txid of the first issue transaction.
Decimals: Byte [0-8] - the number of decimal places. 
Fee: Int - fee offered  to the miners, in wavelets


Upon the asset issue a root hash of corresponding structure is written into the next block, along with the node balance written into the skip list leaf. For the reissuable asset it is needed to also store the issuer address in the skip list, so that in case of reissue the reissue transaction can be validated (Only the asset issuer is able to reissue the asset)


In case of reissue transaction it is allowed to change the “reissuable” flag.


After the issuing transaction is included in the block transaction id of the issuing transaction can also be used as an Asset ID. Since all the nodes have to store all the root hashes of all the assets they store the ID’s of all the assets too.


### Asset transfer


Send(Sequence(AssetID, SenderAddress, ReceiverAddress, Amount), Sequence(Signature))


AssetID: Array[Byte] - txid of the issuing transaction.
Sender Address: Waves address - wallet addresses, asset(s) to be sent
Receiver Address: Option[Waves address] - wallet address, asset(s) to be received. If address is empty this part of Send transaction is the fee to the block miner
Amount. Type: Long
Signature: Array[Byte] -  cryptographic authorisation of the transaction


It should be noted that transaction API call does not contain root hashes or proofs, so the lite (or API) client delegates all the necessary calculations to the full node. Also it should be noted that it is possible to transfer multiple assets in the same transaction from the several accounts to several other accounts. Send transaction should contain signatures for all sender addresses, included in it. This generalized transaction can also include the network token transfer, network token has AssetID 0 in this case.


The total amount of different tokens transferred in one transaction should be limited to prevent spam attacks on the network.


### Asset deletion


Delete(AssetID,Amount,Fee,Signature(s))


AssetID: Array[Byte] - txid of the issuing transaction
Amount: Long - amount of the assets to be deleted.
Fee: Int - fee offered  to the miners, in wavelets
Signature: Array[Byte] -  cryptographic authorisation of the transaction


Any address holding a given asset can choose to destroy some or all of the assets it holds.
The asset state and skip lists are being recalculated based on the Delete transaction.


## Asset exchange operations


In order to make assets useful they have to be exchanged for one another, with the exchange procedure facilitated by the blockchain settlement. WAVES uses an approach that does not put unnecessary data on to the blockchain, instead using the blockchain for settlement, not order matching operations. Order matching is carried out by centralized nodes, after the orders have been matched the matching service creates a swap transaction and sends it to the both counterparties of the exchange operation for signing.


### Placing an exchange order (Limit order)


Order (SpendAddress, MatcherAddress, SpendTokenID, ReceiveTokenID, price, amount, MaxTimestamp,MatcherFee, Signature)


SpendAddress: Waves address - order sender address in WAVES address format. The assets will be spent and sent from/to this address. The matcher checks if the address has a sufficient asset balance
MatcherAddress:  matched orders should be signed by this address
SpendTokenID: Array[Byte] - ID of the token to be spent
ReceiveTokenID: Array[Byte] - ID of the token to be received
Price: Int -how much of the asset to be spent should be paid for the the asset to be received multiplied to 10^8.
Amount: Long - amount of the asset to be received.
MaxTimeStamp - Maximum TimeStamp until which the order is valid
Signature: Array[Byte] -  cryptographic authorisation of the transaction. All transactions have to be signed in order to authenticate the node.  It is considered to be the Order Id.
MatcherFee: Long - Matcher charges a fee for the order matching process. It should be bigger than the fee for asset swap, so Macher can do partial order matching
Matcher responds with a signed confirmation of order acceptance, and begins the matching process. In case if some order parameers are wrong Matcher returns an error message


### Cancelling an exchange order


Cancel (SpendAddress, OrderID, Signature)


SpendAddress: Array[Byte] - order sender address in WAVES address format
OrderID : Long - Order Identificator
Signature: Array[Byte] -  cryptographic authorisation of the transaction. 

If a user wants to cancel her order she sends a Cancel transaction, which should be signed by Matcher. If later this order appears on the blockchain with a higher timestamp  node can submit an order cancelation proof. If the matcher in not trusted a node can send this transaction to blockchain, paying the corresponding fee. In this case all system participants are able to verify that this transaction cannot be accepted.




### Finalizing an exchange transaction


When Matcher finds a matching orders pair to exchange, she sends OrderMatch transaction into the network:

OrderMatch(Order1,Order2, MatcherFee,Fee,TimeStamp,matcherSignature)
Order1, Order2 - Orders of both parties. Network nodes check the correctness of the signatures and its validity at the given time.
price: Long - Order price, nodes check that it corresponds to Order1 and Order2
Amount: Network nodes verify that it corresponds to Order1 and Order2. OrderMatch history should be used for the verification
matcherFee: Long  - Fee sent to the Matcher. Cannot exceed the commission in Order1 and Order2 combined. 
Fee: Long - Miner's fee
TimeStamp: Long - TransactionTime
MatcherSignature: Array[Byte] - Macher signature corresponding to MatcherAddress in Order1, Order2. Network nodes can verify it.

Besides, network nodes verify that spendTokenID1=receiveTokenID2 and spendTokenID2=receiveTokenID1

As a result of this transaction the  balances are modified in the following way:
Matcher recevies fee equal to (matcherFee - fee), in Waves tokens.

Order1 address spends amount / (price / 10^8) assets с id spendTokenID1 and receives amount assets of  id receiveTokenID1
Order2 receives amount / (price / 10^8)  of assets with id spendTokenID1 and spends amount of assets with id receiveTokenID1

The order sent to a Matcher is an order in obligation, that is if it is matched with another order a node that submitted it cannot prevent it from execution. This scheme mimics the way centralized exchanges work, with the only exception being Matcher does not control users' funds.

