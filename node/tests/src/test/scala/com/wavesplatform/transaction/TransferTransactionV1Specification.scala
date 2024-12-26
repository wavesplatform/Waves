package com.wavesplatform.transaction

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.serialization.impl.TransferTxSerializer
import com.wavesplatform.transaction.transfer._
import play.api.libs.json.Json

class TransferTransactionV1Specification extends PropSpec {

  property("Transfer serialization roundtrip") {
    forAll(transferV1Gen) { (transfer: TransferTransaction) =>
      val recovered = TransferTransaction.parseBytes(transfer.bytes()).get

      recovered.sender shouldEqual transfer.sender
      recovered.assetId shouldBe transfer.assetId
      recovered.feeAssetId shouldBe transfer.feeAssetId
      recovered.timestamp shouldEqual transfer.timestamp
      recovered.amount shouldEqual transfer.amount
      recovered.fee shouldEqual transfer.fee
      recovered.recipient shouldEqual transfer.recipient

      recovered.bytes() shouldEqual transfer.bytes()
    }
  }

  property("TransferV2 decode pre-encoded bytes") {
    val bytes = Base58.decode(
      "2vs1kZ8fsY8kznd5FW5zv1XvBjgtNwNW8WS3GP1MC1dHKDVCjLhLV9UgAtVkUP48bXtgFH2TFFsnwqRoJjEVowSHcFqURiDzCZTEU4pKFjXaDmauvmSpN8LPw7VckeQYkAxvpzPpMxhY765wv5zD4sd8oyFeUxVToaNfepstek4ugJFzXZVM4gAqxz5jtiTksxySdNVHRkhgmY3NYxmRFQPenhaXydUWmLAa9xEfjj4gjVPUy47FbFwQUgta3WspKWq1eki6LA4vZtHFZPfcp2DHu9D1KGMSxERmRJpdLRbvD5LbTS8TaXQHUiqTqiafSme829aB1Jdw1s5KXPZhCWEqmv8ryTzHCF3UqnFcEgsi6VKYu1ARDrbUMwB3gbYq4qTW7uhs4qEG348mdEm6CLm1vE5a6ih"
    )
    val json = Json.parse(
      """{
        |  "senderPublicKey" : "45FQmahaQC5BsHYnzLypvTB4YKuzQLytu3m83AcDKn1d",
        |  "amount" : 61305167369911,
        |  "signature" : "3GgwEGeTmHKoZxMyQWRspwk5KfV2RyoE9sbPxxQVgrDXgVjZZbz2Qzyiu2hNHGm2FovYq62YzSMXkzqSmgbTsoEr",
        |  "fee" : 2084965,
        |  "type" : 4,
        |  "version" : 1,
        |  "attachment" : "U7iyYx6HwPHpZjpLKDrkE2LjuF2JAZcjT7aKM3ryzcihToW4FgLqqiUcYbGADz9PMCFXibDCb126RVm4AtoHSzpuW8NDMcAmxZBd2LPiQ3VBuDJacn3dD1X",
        |  "sender" : "3N44LV7DJAi6qxyuMxNsxmsKcGtAD4rXwDV",
        |  "feeAssetId" : "4vKvkk5vseBeaWR1wdjWf8LfWRvzU9SruKwpW2Cvi5u",
        |  "proofs" : [ "3GgwEGeTmHKoZxMyQWRspwk5KfV2RyoE9sbPxxQVgrDXgVjZZbz2Qzyiu2hNHGm2FovYq62YzSMXkzqSmgbTsoEr" ],
        |  "assetId" : "Hrs1iH8YJJKgo1ZgVsqfvFGbRgFp5HxuuU7eCPDPwMjN",
        |  "recipient" : "3N8JoB6QHbKxSFCD2HfhnQXQkibJkBPC4Ag",
        |  "feeAsset" : "4vKvkk5vseBeaWR1wdjWf8LfWRvzU9SruKwpW2Cvi5u",
        |  "id" : "BQTeR8HhbzZfVZ48LFzesi29nPLiKeSGYfesTwWH1pXJ",
        |  "timestamp" : 1133967589140510377
        |}
        |""".stripMargin
    )

    val tx = TransferTxSerializer.parseBytes(bytes)
    tx.get.json() shouldBe json
  }

  property("Transfer serialization from TypedTransaction") {
    forAll(transferV1Gen) { (tx: TransferTransaction) =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON format validation") {
    val js = Json.parse("""{
                        "type": 4,
                        "id": "FLszEaqasJptohmP6zrXodBwjaEYq4jRP2BzdPPjvukk",
                        "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                        "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                        "fee": 100000,
                        "timestamp": 1526552510868,
                        "signature": "eaV1i3hEiXyYQd6DQY7EnPg9XzpAvB9VA3bnpin2qJe4G36GZXaGnYKCgSf9xiQ61DcAwcBFzjSXh6FwCgazzFz",
                        "proofs": ["eaV1i3hEiXyYQd6DQY7EnPg9XzpAvB9VA3bnpin2qJe4G36GZXaGnYKCgSf9xiQ61DcAwcBFzjSXh6FwCgazzFz"],
                        "version": 1,
                        "recipient": "3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8",
                        "assetId": null,
                        "feeAsset":null,
                        "feeAssetId":null,
                        "amount": 1900000,
                        "attachment": "4t2Xazb2SX"
                        }
    """)

    val recipient = Address.fromString("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").explicitGet()
    val tx = TransferTransaction(
      1.toByte,
      PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
      recipient,
      Waves,
      TxPositiveAmount.unsafeFrom(1900000),
      Waves,
      TxPositiveAmount.unsafeFrom(100000),
      ByteStr.decodeBase58("4t2Xazb2SX").get,
      1526552510868L,
      Proofs(Seq(ByteStr.decodeBase58("eaV1i3hEiXyYQd6DQY7EnPg9XzpAvB9VA3bnpin2qJe4G36GZXaGnYKCgSf9xiQ61DcAwcBFzjSXh6FwCgazzFz").get)),
      recipient.chainId
    )

    tx.json() shouldEqual js
  }

  property("negative") {
    for {
      (_, sender, recipient, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransaction.selfSigned(1.toByte, sender, recipient, Waves, amount, Waves, feeAmount, attachment, timestamp) should produce(
      "insufficient fee"
    )
  }
}
