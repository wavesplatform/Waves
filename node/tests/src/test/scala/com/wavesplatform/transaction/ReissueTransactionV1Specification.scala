package com.wavesplatform.transaction

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.ReissueTransaction
import com.wavesplatform.crypto
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.serialization.impl.ReissueTxSerializer
import play.api.libs.json.Json

class ReissueTransactionV1Specification extends PropSpec {

  property("Reissue serialization roundtrip") {
    forAll(reissueGen) { (tx: ReissueTransaction) =>
      val recovered = ReissueTxSerializer.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("ReissueV1 decode pre-encoded bytes") {
    val bytes = Base64.decode(
      "BUaFzzbchvia5UqpkoTJaU0k66PO4BSceYUl8T5XvSoHMUqJmlvrY8b7hD+O0ZGRXpSCjSP3xlO698AVoFFW8YQFMn9IICVa9Lp6GHNSNbHbRRjWTavKmjuHQrHDnvRoVWJfZBDWkMPsh3UtfVipLExxfhHTmO8bF7trMQD/sa1HpAAAHPE6aCBaAAAAAAABZBwmAAAc4w6yh3s="
    )
    val json = Json.parse("""{
                            |  "senderPublicKey" : "4Q7xJbC58HvvSBmfiG9Ybtg79zPKEZSPP8CQf2xnLkc1",
                            |  "quantity" : 31822392598618,
                            |  "fee" : 23338022,
                            |  "type" : 5,
                            |  "version" : 1,
                            |  "reissuable" : false,
                            |  "sender" : "3MyU5WaaGs3zyyWyVG447Ra3GBY96ddVG6q",
                            |  "feeAssetId" : null,
                            |  "signature" : "2Qn9wyJdgCpTFGkXH7Y5jY9HDTQ3ixe7khzAmm7sp5CCUbax4gwjahHdHGrfFxSoWZNTMStvbfXjp9HHCjeFKWMR",
                            |  "proofs" : [ "2Qn9wyJdgCpTFGkXH7Y5jY9HDTQ3ixe7khzAmm7sp5CCUbax4gwjahHdHGrfFxSoWZNTMStvbfXjp9HHCjeFKWMR" ],
                            |  "assetId" : "7RNELbxQa7ihqewsUGQzZdEscZfHJhZPYkUefAsEcvnw",
                            |  "id" : "4hguCDoRV82TxRtzj4QPFKw8ZthMxXcHtKU4jfyg8d8s",
                            |  "timestamp" : 31761529735035
                            |}""".stripMargin)

    val tx = ReissueTxSerializer.parseBytes(bytes).get
    tx.json() shouldBe json
    assert(crypto.verify(tx.signature, tx.bodyBytes(), tx.sender), "signature should be valid")
  }

  property("Reissue serialization from TypedTransaction") {
    forAll(reissueGen) { (tx: ReissueTransaction) =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON format validation") {
    val js = Json.parse("""{
                    "type": 5,
                    "id": "2y8pNQteNQnY5JWtrZGLUv3tD6GFT6DDzBWttVTwBa2t",
                    "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                    "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                    "fee": 100000000,
                    "feeAssetId": null,
                    "timestamp": 1526287561757,
                    "signature": "3LnRMrjkk7RoV35PTwcdB4yW2rqUqXaKAh8DnPk5tNWABvhVQ9oqdTk3zM8b9AbGtry7WEcQZtevfK92DCFaa6hA",
                    "proofs": ["3LnRMrjkk7RoV35PTwcdB4yW2rqUqXaKAh8DnPk5tNWABvhVQ9oqdTk3zM8b9AbGtry7WEcQZtevfK92DCFaa6hA"],
                    "version": 1,
                    "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
                    "quantity": 100000000,
                    "reissuable": true
                    }
    """)

    val tx = ReissueTransaction
      .create(
        1.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get),
        100000000L,
        true,
        100000000L,
        1526287561757L,
        Proofs(ByteStr.decodeBase58("3LnRMrjkk7RoV35PTwcdB4yW2rqUqXaKAh8DnPk5tNWABvhVQ9oqdTk3zM8b9AbGtry7WEcQZtevfK92DCFaa6hA").get)
      )
      .explicitGet()

    js shouldEqual tx.json()
  }

}
