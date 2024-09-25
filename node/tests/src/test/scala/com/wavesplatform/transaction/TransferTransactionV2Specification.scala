package com.wavesplatform.transaction

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.serialization.impl.TransferTxSerializer
import com.wavesplatform.transaction.transfer._
import play.api.libs.json.Json

class TransferTransactionV2Specification extends PropSpec {

  property("VersionedTransferTransactionSpecification serialization roundtrip") {
    forAll(transferV2Gen) { (tx: TransferTransaction) =>
      val recovered = TransferTransaction.parseBytes(tx.bytes()).get
      assertTxs(recovered, tx)
    }
  }

  property("TransferV2 decode pre-encoded bytes") {
    val bytes = Base58.decode(
      "1BQcrqKmQy9vV8TxNbQA5VWKdFswHNUhPaCuKfwGPuZ1QPMgnkD6zkyuNnGiMpx2X8AUcVFiN6hYZ4SYF8Kxf8FGw8UGSTSJfv9xepxe1HbsUxGvL2zuwJ1z29mtZTunwKkAnMhqfFL6zWmQApmy4d2RYfyyW8StFaREmmP7KfG8T7Hv89r87TMaMQ2hvcLG94tzWKmLjhM4dXqJTgvNHcW6SxChSEn9mRbwu9Xu3jpFERTM9LJCbBhVAWnoz2ySAPCfHzF5otGEQjdyenuzR3av2EVg5UFd8bwkvXDUPTgyQqFMaStChZfSjzF5Kyt1vfaYMXpPpqh3ppERMx3ivCfGoStF2M9cksANC7cGPULopjSZXQY5R78djAFA87jUfy8fCoUvc7dRw84R5o7hXZ"
    )
    val json = Json.parse(
      """{
        |  "senderPublicKey" : "DVCo1PdepfbhA9xkRTtPc69osp86DyM9FWEp8TwGmNb1",
        |  "amount" : 80901858834201,
        |  "fee" : 22715602,
        |  "type" : 4,
        |  "version" : 2,
        |  "attachment" : "",
        |  "sender" : "3MzrGSzM8FhSJzgJtUzpCdxVEX4hpH5Mz3Z",
        |  "feeAssetId" : "12xeUNhM2T9ZxXrdCsXmWHEUo3rPecRJWvqhkYzcG9MV",
        |  "proofs" : [ "1TZU9cySE3Jsutd1tJRx3Ac", "6s2KJreWUbBc4XLhbTkkpnGEJc2oPbBVqK9pCByqwnj", "ngYbR6KbDZnxu3T9NCN4MhF4DwgT41vqGmzFBfzMfb6RiJPrWUoeLnS6LTTU", "V5dyBx3qohQsxBk7KjA7niMELR42nvowbz8NwjseqS48r7XFWaBfEuf9" ],
        |  "assetId" : "61spwGWBiKn2eXLqVjE3i3KdhAk3Mu5GiRW14vF2sMhF",
        |  "recipient" : "3N3dGRLoei6N6EqxBnoCWMveFrS5CtmDwXK",
        |  "feeAsset" : "12xeUNhM2T9ZxXrdCsXmWHEUo3rPecRJWvqhkYzcG9MV",
        |  "id" : "3ETD1ZFwiENauzF8Av4J818juEX8w7AS3ku3k3KSZR77",
        |  "timestamp" : 4512675353884576883
        |}
        |""".stripMargin
    )

    val tx = TransferTxSerializer.parseBytes(bytes)
    tx.get.json() shouldBe json
  }

  property("VersionedTransferTransactionSpecification serialization from TypedTransaction") {
    forAll(transferV2Gen) { (tx: TransferTransaction) =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[TransferTransaction], tx)
    }
  }

  property("VersionedTransferTransactionSpecification id doesn't depend on proof") {
    forAll(accountGen, accountGen, proofsGen, proofsGen, attachmentGen) { case (_, acc2, proofs1, proofs2, attachment) =>
      val tx1 = TransferTransaction(
        2.toByte,
        acc2.publicKey,
        acc2.toAddress,
        Waves,
        TxPositiveAmount.unsafeFrom(1),
        Waves,
        TxPositiveAmount.unsafeFrom(1),
        attachment,
        1,
        proofs1,
        acc2.toAddress.chainId
      )
      val tx2 = TransferTransaction(
        2.toByte,
        acc2.publicKey,
        acc2.toAddress,
        Waves,
        TxPositiveAmount.unsafeFrom(1),
        Waves,
        TxPositiveAmount.unsafeFrom(1),
        attachment,
        1,
        proofs2,
        acc2.toAddress.chainId
      )
      tx1.id() shouldBe tx2.id()
    }
  }

  private def assertTxs(first: TransferTransaction, second: TransferTransaction): Unit = {
    first.sender shouldEqual second.sender
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.amount shouldEqual second.amount
    first.recipient shouldEqual second.recipient
    first.version shouldEqual second.version
    first.assetId shouldEqual second.assetId
    first.feeAssetId shouldEqual second.feeAssetId
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

  property("JSON format validation") {
    val js = Json.parse("""{
                       "type": 4,
                       "id": "2qMiGUpNMuRpeyTnXLa1mLuVP1cYEtxys55cQbDaXd5g",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000000,
                       "timestamp": 1526641218066,
                       "proofs": [
                       "4bfDaqBcnK3hT8ywFEFndxtS1DTSYfncUqd4s5Vyaa66PZHawtC73rDswUur6QZu5RpqM7L9NFgBHT1vhCoox4vi"
                       ],
                       "version": 2,
                       "recipient": "3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8",
                       "assetId": null,
                       "feeAsset": null,
                       "feeAssetId":null,
                       "amount": 100000000,
                       "attachment": "4t2Xazb2SX"}
    """)

    val recipient = Address.fromString("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").explicitGet()
    val tx = TransferTransaction(
      2.toByte,
      PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
      Address.fromString("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").explicitGet(),
      Waves,
      TxPositiveAmount.unsafeFrom(100000000),
      Waves,
      TxPositiveAmount.unsafeFrom(100000000),
      ByteStr.decodeBase58("4t2Xazb2SX").get,
      1526641218066L,
      Proofs(Seq(ByteStr.decodeBase58("4bfDaqBcnK3hT8ywFEFndxtS1DTSYfncUqd4s5Vyaa66PZHawtC73rDswUur6QZu5RpqM7L9NFgBHT1vhCoox4vi").get)),
      recipient.chainId
    )

    tx.json() shouldEqual js
  }
}
