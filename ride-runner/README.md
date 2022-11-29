* Ride runner

** Limitations

We have some limitations. If you faced one of them, please issue a ticket on GitHub and tell us your use-case.

1. [transferTransactionById](https://docs.waves.tech/en/ride/functions/built-in-functions/blockchain-functions#transfertransactionbyid)
   is not supported. A script fails with an error if tries to run this function.
2. [wavesBalance](https://docs.waves.tech/en/ride/functions/built-in-functions/blockchain-functions#wavesbalance)
   returns [BalanceDetails](https://docs.waves.tech/en/ride/structures/common-structures/balance-details), which has a
   wrong `generating` value for now.
3. [isDataStorageUntouched](https://docs.waves.tech/en/ride/functions/built-in-functions/account-data-storage-functions#isdatastorageuntouched-address-alias-boolean)
   is not supported.
4. Asset scripts aren't supported as in `GET /utils/script/evaluate` of Node REST API. 
 