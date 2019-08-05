package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.{PublicKey, Address}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.state.diffs._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer._
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.Json

class TransferTransactionV1Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Transfer serialization roundtrip") {
    forAll(transferV1Gen) { transfer: TransferTransactionV1 =>
      val recovered = TransferTransactionV1.parseBytes(transfer.bytes()).get

      recovered.sender.stringRepr shouldEqual transfer.sender.stringRepr
      recovered.assetId shouldBe transfer.assetId
      recovered.feeAssetId shouldBe transfer.feeAssetId
      recovered.timestamp shouldEqual transfer.timestamp
      recovered.amount shouldEqual transfer.amount
      recovered.fee shouldEqual transfer.fee
      recovered.recipient.stringRepr shouldEqual transfer.recipient.stringRepr

      recovered.bytes() shouldEqual transfer.bytes()
    }
  }

  property("Transfer serialization from TypedTransaction") {
    forAll(transferV1Gen) { tx: TransferTransactionV1 =>
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

    val tx = TransferTransactionV1
      .create(
        Waves,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Address.fromString("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").explicitGet(),
        1900000,
        1526552510868L,
        Waves,
        100000,
        Base58.tryDecodeWithLimit("4t2Xazb2SX").get,
        ByteStr.decodeBase58("eaV1i3hEiXyYQd6DQY7EnPg9XzpAvB9VA3bnpin2qJe4G36GZXaGnYKCgSf9xiQ61DcAwcBFzjSXh6FwCgazzFz").get
      )
      .right
      .get

    tx.json() shouldEqual js
  }

  property("negative") {
    for {
      (_, sender, recipient, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
      sender                                                              <- accountGen
    } yield
      TransferTransactionV1.selfSigned(Waves, sender, recipient, amount, timestamp, Waves, feeAmount, attachment) should produce("insufficient fee")
  }
}
