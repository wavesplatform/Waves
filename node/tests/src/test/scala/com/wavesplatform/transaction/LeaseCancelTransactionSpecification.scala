package com.wavesplatform.transaction

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import com.wavesplatform.transaction.serialization.impl.LeaseCancelTxSerializer
import play.api.libs.json.Json

class LeaseCancelTransactionSpecification extends PropSpec {

  property("Lease cancel serialization roundtrip") {
    forAll(leaseCancelGen) { (tx: LeaseCancelTransaction) =>
      val recovered = LeaseCancelTxSerializer.parseBytes(tx.bytes()).get
      assertTxs(recovered, tx)
    }
  }

  property("Lease cancel decode pre-encoded bytes") {
    val bytes = Base58.decode(
      "3DyJ39VY7RW3qpveA82NJhLS4YFiJTZ84Lg4r4fA1W75kPxtkJXk5EZ8kBRGUpsK5Mws77JQiraoLgvyLH4KixRH6ZWUbRFhD7HfZuiQrzNxGpfdXqHRXLQk91c7SqRgMfrop6McMds2NQ8vQxJRfntFwi3xVq2NTHfzQdwapcuaBt3jyoTgqjTth1WTzYwkXPApjW"
    )
    val json = Json.parse("""{
                            |  "senderPublicKey" : "Zn6fENXpGbd68Pd8gH6YYTL7mBuUZvd6YG3AZfckPmN",
                            |  "leaseId" : "2o9jDyHJeAjj4JmBCWoCWX1W494gcxCBifTw4GE7mijN",
                            |  "sender" : "3N6Jpv3nXAcbYvziaK3bGDjGsfo5XtJ4ti5",
                            |  "feeAssetId" : null,
                            |  "signature" : "4cZVxhxZCGgwi2AqWScZ3vft6DPKsRyXBMAr4M6sjj2hUJH5DSfDnC1aQYBv5kvqeQfWt3NggdB6wAvfARK85zy2",
                            |  "proofs" : [ "4cZVxhxZCGgwi2AqWScZ3vft6DPKsRyXBMAr4M6sjj2hUJH5DSfDnC1aQYBv5kvqeQfWt3NggdB6wAvfARK85zy2" ],
                            |  "fee" : 66288378,
                            |  "id" : "DYPzB2aupCtHMrP2YLJXnEr74XpJii5SVMmwPH5Cz3dE",
                            |  "type" : 9,
                            |  "version" : 1,
                            |  "timestamp" : 8263749264550800915
                            |}""".stripMargin)

    val tx = LeaseCancelTxSerializer.parseBytes(bytes)
    tx.get.json() shouldBe json
  }

  property("Lease cancel serialization from TypedTransaction") {
    forAll(leaseCancelGen) { (tx: LeaseCancelTransaction) =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[LeaseCancelTransaction], tx)
    }
  }

  private def assertTxs(first: LeaseCancelTransaction, second: LeaseCancelTransaction): Unit = {
    first.leaseId shouldEqual second.leaseId
    first.fee shouldEqual second.fee
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

  property("JSON format validation for LeaseCancelTransactionV1") {
    val js = Json.parse("""{
                       "type": 9,
                       "id": "7hmabbFS8a2z79a29pzZH1s8LHxrsEAnnLjJxNdZ1gGw",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 1000000,
                       "feeAssetId": null,
                       "timestamp": 1526646300260,
                       "signature": "4T76AXcksn2ixhyMNu4m9UyY54M3HDTw5E2HqUsGV4phogs2vpgBcN5oncu4sbW4U3KU197yfHMxrc3kZ7e6zHG3",
                       "proofs": ["4T76AXcksn2ixhyMNu4m9UyY54M3HDTw5E2HqUsGV4phogs2vpgBcN5oncu4sbW4U3KU197yfHMxrc3kZ7e6zHG3"],
                       "version": 1,
                       "leaseId": "EXhjYjy8a1dURbttrGzfcft7cddDnPnoa3vqaBLCTFVY"
                       }
    """)

    val tx = LeaseCancelTransaction
      .create(
        1.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        ByteStr.decodeBase58("EXhjYjy8a1dURbttrGzfcft7cddDnPnoa3vqaBLCTFVY").get,
        1000000,
        1526646300260L,
        Proofs(ByteStr.decodeBase58("4T76AXcksn2ixhyMNu4m9UyY54M3HDTw5E2HqUsGV4phogs2vpgBcN5oncu4sbW4U3KU197yfHMxrc3kZ7e6zHG3").get)
      )
      .explicitGet()

    js shouldEqual tx.json()
  }

  property("JSON format validation for LeaseCancelTransactionV2") {
    val js = Json.parse("""{
                        "type": 9,
                        "id": "4nvUUiQjTH7D2LFyzaxs8JwaZYZHDggJgq1iP99TvVDM",
                        "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                        "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                        "fee": 1000000,
                        "feeAssetId":null,
                        "timestamp": 1526646300260,
                        "proofs": [
                        "3h5SQLbCzaLoTHUeoCjXUHB6qhNUfHZjQQVsWTRAgTGMEdK5aeULMVUfDq63J56kkHJiviYTDT92bLGc8ELrUgvi"
                        ],
                        "version": 2,
                        "leaseId": "DJWkQxRyJNqWhq9qSQpK2D4tsrct6eZbjSv3AH4PSha6",
                        "chainId": 84
                       }
    """)

    val tx = LeaseCancelTransaction
      .create(
        2.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        ByteStr.decodeBase58("DJWkQxRyJNqWhq9qSQpK2D4tsrct6eZbjSv3AH4PSha6").get,
        1000000,
        1526646300260L,
        Proofs(Seq(ByteStr.decodeBase58("3h5SQLbCzaLoTHUeoCjXUHB6qhNUfHZjQQVsWTRAgTGMEdK5aeULMVUfDq63J56kkHJiviYTDT92bLGc8ELrUgvi").get))
      )
      .explicitGet()

    js shouldEqual tx.json()
  }

}
