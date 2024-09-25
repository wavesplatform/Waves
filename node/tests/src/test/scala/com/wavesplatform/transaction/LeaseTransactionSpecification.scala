package com.wavesplatform.transaction

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.serialization.impl.LeaseTxSerializer
import play.api.libs.json.Json

class LeaseTransactionSpecification extends PropSpec {

  property("Lease transaction serialization roundtrip") {
    forAll(leaseGen) { (tx: LeaseTransaction) =>
      val recovered = LeaseTxSerializer.parseBytes(tx.bytes()).get
      assertTxs(recovered, tx)
    }
  }

  property("Lease decode pre-encoded bytes") {
    val bytes = Base58.decode(
      "17RZzWQkD4AuKFYXkzg66BTVv7Fwp8nS3kNdmdkM3NfSFCVb9ZndDksiafmwrSDzAXmSZF7bZ7MnCzXdVopLrMnMUTa4E9Y48xCWn1op36wTCQppVD6VEYovPTm9UyVCTtWsGrA6Wk9sHXTaw9T1vhV76Mcsoq2zXe2CUBwCGnNxevSBGNSNp7EmnT7iVzVRsu1TKFFUdYVy9T6UnLc"
    )
    val json = Json.parse("""
                            |{
                            |  "senderPublicKey" : "D2gYkCwGYjoYhUTBTNzexhwRTtvoaKahytk7RwuUwH5d",
                            |  "amount" : 5023249666181,
                            |  "sender" : "3NA5AnR3UHvZQNuxPrVU4EBABn9vmMoL4cx",
                            |  "feeAssetId" : null,
                            |  "proofs" : [ "2CEU2jY9ExM7y1WRYp7EjREG3JkBhy87UUhu4UpUZxePpH3iqRNcBXx6L6rRnLxCnqJrXuGpWPJDc1LBnRRCtG6g" ],
                            |  "fee" : 34738831,
                            |  "recipient" : "3N1HWMWFprz1RkF9KQggeVGyvKn7SGJoqrt",
                            |  "id" : "CiqQ1F74XnEuE23rAN7r29vRQ78L5WDNSEeW4dE2bvrg",
                            |  "type" : 8,
                            |  "version" : 2,
                            |  "timestamp" : 5173516202896675941
                            |}
                            |""".stripMargin)

    val tx = LeaseTxSerializer.parseBytes(bytes)
    tx.get.json() shouldBe json
  }

  property("Lease transaction from TransactionParser") {
    forAll(leaseGen) { (tx: LeaseTransaction) =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[LeaseTransaction], tx)
    }
  }

  private def assertTxs(first: LeaseTransaction, second: LeaseTransaction): Unit = {
    first.sender shouldEqual second.sender
    first.recipient shouldEqual second.recipient
    first.amount shouldEqual second.amount
    first.fee shouldEqual second.fee
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

  property("JSON format validation for LeaseTransactionV1") {
    val js = Json.parse("""{
                       "type": 8,
                       "id": "EXhjYjy8a1dURbttrGzfcft7cddDnPnoa3vqaBLCTFVY",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 1000000,
                       "feeAssetId": null,
                       "timestamp": 1526646300260,
                       "signature": "iy3TmfbFds7pc9cDDqfjEJhfhVyNtm3GcxoVz8L3kJFvgRPUmiqqKLMeJGYyN12AhaQ6HvE7aF1tFgaAoCCgNJJ",
                       "proofs": ["iy3TmfbFds7pc9cDDqfjEJhfhVyNtm3GcxoVz8L3kJFvgRPUmiqqKLMeJGYyN12AhaQ6HvE7aF1tFgaAoCCgNJJ"],
                       "version": 1,
                       "amount": 10000000,
                       "recipient": "3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd"
                       }
    """)

    val tx = LeaseTransaction
      .create(
        1.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Address.fromString("3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd").explicitGet(),
        10000000,
        1000000,
        1526646300260L,
        Proofs(ByteStr.decodeBase58("iy3TmfbFds7pc9cDDqfjEJhfhVyNtm3GcxoVz8L3kJFvgRPUmiqqKLMeJGYyN12AhaQ6HvE7aF1tFgaAoCCgNJJ").get)
      )
      .explicitGet()

    js shouldEqual tx.json()
  }

  property("JSON format validation for LeaseTransactionV2") {
    val js = Json.parse("""{
                        "type": 8,
                        "id": "UL85wuJDXXe6BtQUob4KNb72kTaf8RN9Gp1NajvGMeU",
                        "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                        "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                        "fee": 1000000,
                        "feeAssetId": null,
                        "timestamp": 1526646497465,
                        "proofs": [
                        "5Fr3yLwvfKGDsFLi8A8JbHqToHDojrPbdEGx9mrwbeVWWoiDY5pRqS3rcX1rXC9ud52vuxVdBmGyGk5krcgwFu9q"
                        ],
                        "version": 2,
                        "amount": 10000000,
                        "recipient": "3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd"
                       }
    """)

    val tx = LeaseTransaction
      .create(
        2.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Address.fromString("3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd").explicitGet(),
        10000000,
        1000000,
        1526646497465L,
        Proofs(Seq(ByteStr.decodeBase58("5Fr3yLwvfKGDsFLi8A8JbHqToHDojrPbdEGx9mrwbeVWWoiDY5pRqS3rcX1rXC9ud52vuxVdBmGyGk5krcgwFu9q").get))
      )
      .explicitGet()

    js shouldEqual tx.json()
  }

  property("forbid assetId in LeaseTransactionV2") {
    val leaseV2Gen      = leaseGen.filter(_.version == 2)
    val assetIdBytesGen = bytes32gen
    forAll(leaseV2Gen, assetIdBytesGen) { (tx, assetId) =>
      val bytes = tx.bytes()
      // hack in an assetId
      bytes(3) = 1: Byte
      val bytesWithAssetId = bytes.take(4) ++ assetId ++ bytes.drop(4)
      val parsed           = LeaseTxSerializer.parseBytes(bytesWithAssetId)
      parsed.isFailure shouldBe true
      parsed.failed.get.getMessage.contains("Leasing assets is not supported yet") shouldBe true
    }
  }
}
