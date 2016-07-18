Waves asset specification




1. Goals


Waves assets combine scalability enhancements on technical level with usability and versatility on business level. 


In order to provide scalability in the system core a simplified payment verification scheme based on authenticated data structures is used.  In a balance-based ledger such as Waves a current balance of all the tokens have to be constructed by network nodes using the blockchain history. This  creates storage, scalability and performance issues.


It is suggested to make use of the fact that most of the the assets won’t have a very high transaction volume, so only a small share of all the assets will be used actively.  Thus it makes sense to store only states of certain top volume assets, and update it using simplified proofs, conceptually similar to SPV lite client verification in Bitcoin.


Lite clients are able to verify the assets transactions without even storing the partial system states.
To that end they can validate the transactions in the new block using root hash of the previous asset state in the last block, root hash in the new block, and the transaction proof.


State storage reduction is achieved through putting additional information into the blockchain - root hash of the authenticated data structure for the state of the given asset and the cryptographic proof of the asset balance for the given address. It can be shown that the resulting block size increase is manageable due to the usage of compact data structures.


2. API


2.1  Asset creation, deletion, and transfer operation.


2.1.1 Asset creation.


Issue (AssetName, Description, Quantity, Decimals, Reissuable, AssetID) 


AssetName - asset identificator. Does not have to be unique. Type: String [4-16]
Description -  asset description text.  Type: String [0-1000]
Quantity - quantity of the assets issued.   Type: Long Integer. The decimal places have to be taken into account, that is Quantity is multiplied by the number of decimal places on API level in order to be able to work only with integer values.
Reissuable - flag which determines if additional assets can be issued later. Type: Boolean
AssetID - in case of reissue of a reissuable asset txid of the first issue transaction. Type: String
Decimals - the number of decimal places. Type: Int [0-8] 


Upon the asset issue a root hash of corresponding structure is written into the next block, along with the node balance written into the skip list leaf. For the reissuable asset it is needed to also store the issuer address in the skip list, so that in case of reissue the reissue transaction can be validated (Only the asset issuer is able to reissue the asset)


In case of reissue transaction it is allowed to change the “reissuable” flag.


After the issuing transaction is included in the block transaction id of the issuing transaction can also be used as an Asset ID. Since all the nodes have to store all the root hashes of all the assets they store the ID’s of all the assets too.


2.1.2 Asset transfer


Send(AssetName(s), SenderAddress(es), ReceiverAddress(es), Amount(s), Fee, Signature(s))


AssetName - asset name. Type: String [4-16]
AssetID - txid of the issuing transaction Type: String
Sender and Receiver Address -  Type: Waves addresses
Amount. Type: Long Integer
Fee - is determined by the sender, in wavelets. Type: Integer
Signature -  cryptographic authorisation of the transaction Type: String


It should be noted that transaction API call does not contain root hashes or proofs, so the lite (or API) client delegates all the necessary calculations to the full node. Also it should be noted that it is possible to transfer multiple assets in the same transaction from the several accounts to several other accounts. This generalized transaction can include the network token transfer too, network token has AssetID 0 in this case.


The total amount of different tokens transferred in one transaction should be limited to prevent spam attacks on the network.


2.1.3 Asset deletion


Delete(AssetID,Amount,Fee,Signature(s))


AssetID - txid of the issuing transaction Type: String
Amount - amount of the assets to be deleted. Type: Int
Fee - fee offered  to the miners, in wavelets Type: Int
Signature -  cryptographic authorisation of the transaction Type: String


Any address holding a given asset can choose to destroy some or all of the assets it holds.
The asset state and skip lists are being recalculated based on the Delete transaction.


2.2 Asset exchange operations


In order to make assets useful they have to be exchanged for one another, with the exchange procedure facilitated by the blockchain settlement. WAVES uses an approach which does not put unnecessary data on to the blockchain, instead using the blockchain for settlement, not order matching operations. Order matching is carried out by centralized nodes, after the orders have been matched the matching service creates a swap transaction and sends it to the both counterparties of the exchange operation for signing.


2.2.1 Placing an exchange order


Order (SpendAddress, SpendTokenID, ReceiveTokenID, price, amount, Signature)


SpendAddress - order sender address in WAVES address format
SpendTokenID - ID of the token to be spent
ReceiveTokenID - ID of the token to be received
Price - how much of the asset to be spent should be paid for the the asset to be received. Type: Float
Amount - amount of the asset to be received. Type: Long
Signature -  cryptographic authorisation of the transaction. All transactions have to be signed in order to authenticate the node. Type: String  


Order API call is not finalized on the blockchain. In order to be able to control the orders Order API call has to return OrderID, which is stored by the client. OrderID is constructed as a hash of Order transaction.


2.2.1 Cancelling an exchange order


Cancel (SpendAddress, OrderID, Signature)


SpendAddress - order sender address in WAVES address format
OrderID - Order Identificator Type: Long
Signature -  cryptographic authorisation of the transaction. 




2.2.2 Finalizing an exchange transaction


Send (SpendTokenID, ReceiveTokenID, SpendAddress, ReceiveAddress,  MatcherAddres, SpendTokenAmount,ReceiveTokenAmount, MatcherFee, SignatureSpender,
SignatureReceiver)
SpendTokenID, ReceiveTokenID, SpendAddress, ReceiveAddress  are set to ensure the correct exchange direction.
SpendTokenAmount, ReceiveTokenAmount  are the result of the matching operation. They can be validated by the both parties of the exchange, if they find the amount to be incorrect they may choose not to sign the transaction.
MatcherAddress - Matching service Waves address for the fee payment. Matching service can impose a certain fee for its services.
MatcherFee - Amount to be paid to the matching service.
SignatureSpender, SignatureReceiver - signatures of the both parties. The transaction has to be signed by both counterparties in order to be finalized on the blockchain. 


“Send” transaction is used for blockchain order settlement, no special transaction type is created.


Upon signing by both parties the transaction is transmitted by the matching service into the network to be included in the next block.