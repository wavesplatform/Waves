package com.wavesplatform.transaction

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.BurnTransaction
import com.wavesplatform.crypto
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.serialization.impl.BurnTxSerializer
import play.api.libs.json.Json

class BurnTransactionSpecification extends PropSpec {
  property("Burn serialization roundtrip") {
    forAll(burnGen) { tx =>
      val recovered = BurnTxSerializer.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("Burn decode pre-encoded bytes") {
    val bytes = Base64.decode(
      "AAYCVMF50qu1ZfpSEEGAlzsPlJ2CXg6d1rpGF0nJ4kAdFutRpuVLZNIrPMBrp8njB25S3GlA2QoqaDrMQCSB2Z0fXBwAAB+BPnriJwAAAAABV1y0AAA0Lmcgr3gBAAEAQIVRxwoH4ktIQf1K/mmAZHy68IPBuYqIeIGJILpO2+mTcKjvR/+PUc0FLQ6ae+zvclqaqg4QVGxWQVXLJozDq48="
    )
    val json = Json.parse("""{
                            |  "senderPublicKey" : "E2FRjhjyZdivKG3BsU2wf51qXnRjyuY3ks6c5Pc92CpQ",
                            |  "amount" : 34639959482919,
                            |  "sender" : "3N9MZbExso5wtm1sPXwhSHxFkzrC7svcEVv",
                            |  "feeAssetId" : null,
                            |  "chainId" : 84,
                            |  "proofs" : [ "3fbgfBuU4tyb9wbBVKnG3BQLG8tdYhfroyXzrqTtXFCKXpGTBVZahai3iWgxTKpkvrkUCysvtYuT1RNjSVyKSnWa" ],
                            |  "assetId" : "CEVU6Ad1m3FhDMEGKJeeYZU4MzXRtuovCUMgKiLLcsKy",
                            |  "fee" : 22502580,
                            |  "id" : "DkyvbeeSAEAWu5RHtPoVY3pgnJzt9hXXyd5e3J6PcT3p",
                            |  "type" : 6,
                            |  "version" : 2,
                            |  "timestamp" : 57373903335288
                            |}""".stripMargin)

    val tx = BurnTransaction.serializer.parseBytes(bytes).get
    tx.json() shouldBe json
    assert(crypto.verify(tx.signature, tx.bodyBytes(), tx.sender), "signature should be valid")
  }

  property("Burn serialization from TypedTransaction") {
    forAll(burnGen) { (issue: BurnTransaction) =>
      val recovered = TransactionParsers.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("JSON format validation for BurnTransactionV1") {
    val js = Json.parse("""{
                       "type": 6,
                       "id": "Ci1q7y7Qq2C2GDH7YVXsQ8w5vRRKYeoYTp9J76AXw8TZ",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000000,
                       "timestamp": 1526287561757,
                       "signature": "uapJcAJQryBhWThU43rYgMNmvdT7kY747vx5BBgxr2KvaeTRx8Vsuh4yu1JxBymU9LnAoo1zjQcPrWSuhi6dVPE",
                       "proofs": ["uapJcAJQryBhWThU43rYgMNmvdT7kY747vx5BBgxr2KvaeTRx8Vsuh4yu1JxBymU9LnAoo1zjQcPrWSuhi6dVPE"],
                       "version": 1,
                       "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
                       "feeAssetId": null,
                       "amount": 10000000000
                    }
    """)

    val tx = BurnTransaction
      .create(
        1.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get),
        10000000000L,
        100000000L,
        1526287561757L,
        Proofs(ByteStr.decodeBase58("uapJcAJQryBhWThU43rYgMNmvdT7kY747vx5BBgxr2KvaeTRx8Vsuh4yu1JxBymU9LnAoo1zjQcPrWSuhi6dVPE").get)
      )
      .explicitGet()
    js shouldEqual tx.json()
  }

  property("JSON format validation for BurnTransactionV2") {
    val js = Json.parse("""{
                       "type": 6,
                       "id": "6QA1sLV53euVCX5fFemNuEyRVdQ5JYo5dWDsCmtKADRc",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000000,
                       "timestamp": 1526287561757,
                       "proofs": [
                       "3NcEv6tcVMuXkTJwiqW4J3GMCTe8iSLY7neEfNZonp59eTQEZXYPQWs565CRUctDrvcbtmsRgWvnN7BnFZ1AVZ1H"
                       ],
                       "chainId": 84,
                       "version": 2,
                       "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
                       "feeAssetId": null,
                       "amount": 10000000000
                    }
    """)

    val tx = BurnTransaction
      .create(
        2.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get),
        10000000000L,
        100000000L,
        1526287561757L,
        Proofs(Seq(ByteStr.decodeBase58("3NcEv6tcVMuXkTJwiqW4J3GMCTe8iSLY7neEfNZonp59eTQEZXYPQWs565CRUctDrvcbtmsRgWvnN7BnFZ1AVZ1H").get))
      )
      .explicitGet()

    js shouldEqual tx.json()
  }

}
