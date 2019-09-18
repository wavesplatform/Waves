package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.transfer.MassTransferTransaction.{MaxTransferCount, ParsedTransfer, Transfer}
import com.wavesplatform.transaction.transfer._
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.Json

import scala.util.Success

class MassTransferTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("serialization roundtrip") {
    forAll(massTransferGen) { tx: MassTransferTransaction =>
      require(tx.bytes().head == MassTransferTransaction.typeId)
      val recovered = MassTransferTransaction.parseBytes(tx.bytes()).get

      recovered.sender.stringRepr shouldEqual tx.sender.stringRepr
      recovered.assetId shouldBe tx.assetId
      recovered.timestamp shouldEqual tx.timestamp
      recovered.fee shouldEqual tx.fee

      recovered.transfers.zip(tx.transfers).foreach {
        case (ParsedTransfer(rr, ra), ParsedTransfer(tr, ta)) =>
          rr shouldEqual tr
          ra shouldEqual ta
      }

      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("serialization from TypedTransaction") {
    forAll(massTransferGen) { tx: MassTransferTransaction =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("property validation") {
    import MassTransferTransaction.create

    forAll(massTransferGen) {
      case MassTransferTransaction(assetId, sender, transfers, timestamp, fee, attachment, proofs) =>
        val tooManyTransfers   = List.fill(MaxTransferCount + 1)(ParsedTransfer(sender.toAddress, 1L))
        val tooManyTransfersEi = create(assetId, sender, tooManyTransfers, timestamp, fee, attachment, proofs)
        tooManyTransfersEi shouldBe Left(GenericError(s"Number of transfers ${tooManyTransfers.length} is greater than $MaxTransferCount"))

        val negativeTransfer   = List(ParsedTransfer(sender.toAddress, -1L))
        val negativeTransferEi = create(assetId, sender, negativeTransfer, timestamp, fee, attachment, proofs)
        negativeTransferEi shouldBe Left(GenericError("One of the transfers has negative amount"))

        val oneHalf    = Long.MaxValue / 2 + 1
        val overflow   = List.fill(2)(ParsedTransfer(sender.toAddress, oneHalf))
        val overflowEi = create(assetId, sender, overflow, timestamp, fee, attachment, proofs)
        overflowEi shouldBe Left(TxValidationError.OverflowError)

        val feeOverflow   = List(ParsedTransfer(sender.toAddress, oneHalf))
        val feeOverflowEi = create(assetId, sender, feeOverflow, timestamp, oneHalf, attachment, proofs)
        feeOverflowEi shouldBe Left(TxValidationError.OverflowError)

        val longAttachment   = Array.fill(TransferTransaction.MaxAttachmentSize + 1)(1: Byte)
        val longAttachmentEi = create(assetId, sender, transfers, timestamp, fee, longAttachment, proofs)
        longAttachmentEi shouldBe Left(TxValidationError.TooBigArray)

        val noFeeEi = create(assetId, sender, feeOverflow, timestamp, 0, attachment, proofs)
        noFeeEi shouldBe Left(TxValidationError.InsufficientFee())

        val negativeFeeEi = create(assetId, sender, feeOverflow, timestamp, -100, attachment, proofs)
        negativeFeeEi shouldBe Left(TxValidationError.InsufficientFee())
    }
  }

  property("JSON format validation") {
    val js = Json.parse("""{
                       "type": 11,
                       "id": "H36CTJc7ztGRZPCrvpNYeagCN1HV1gXqUthsXKdBT3UD",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 200000,
                       "feeAssetId": null,
                       "timestamp": 1518091313964,
                       "proofs": [
                       "FXMNu3ecy5zBjn9b69VtpuYRwxjCbxdkZ3xZpLzB8ZeFDvcgTkmEDrD29wtGYRPtyLS3LPYrL2d5UM6TpFBMUGQ"],
                       "version": 1,
                       "assetId": null,
                       "attachment": "59QuUcqP6p",
                       "transferCount": 2,
                       "totalAmount": 300000000,
                       "transfers": [
                       {
                       "recipient": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "amount": 100000000
                       },
                       {
                       "recipient": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "amount": 200000000
                       }
                       ]
                       }
  """)

    val transfers = MassTransferTransaction
      .parseTransfersList(
        List(Transfer("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", 100000000L), Transfer("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", 200000000L))
      )
      .right
      .get

    val tx = MassTransferTransaction
      .create(
        Waves,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        transfers,
        1518091313964L,
        200000,
        Base58.tryDecodeWithLimit("59QuUcqP6p").get,
        Proofs(Seq(ByteStr.decodeBase58("FXMNu3ecy5zBjn9b69VtpuYRwxjCbxdkZ3xZpLzB8ZeFDvcgTkmEDrD29wtGYRPtyLS3LPYrL2d5UM6TpFBMUGQ").get))
      )
      .right
      .get

    js shouldEqual tx.json()
  }

  property("empty transfers validation") {
    import play.api.libs.json._
    val transaction = TransactionFactory
      .fromSignedRequest(
        Json.parse(
          """{"senderPublicKey":"CkvZ3sY9o8zV1akquJk6Y5d9Ke4G68zGzfSTep1KZBhi","fee":51449505,"type":11,"transferCount":0,"version":1,"totalAmount":0,"attachment":"SQqypFfuVh4j4H6zaZaAXPT8fbuNQSo6cvv6jiQuYeFXDJjtjwvu7QE6bAzQJ7VSCpX8km6rTYbug7mi3i","sender":"3MrFBnLCGKibu1jrEcRiHqGBwYCeQgyUQHc","feeAssetId":null,"proofs":["uYVrE8fKzR2dcx1EXU2WLWPwgWfdgAizRuwDJd2eU19rgoSpPuUF9eQCQzmKxg1pA3Tcp31W9MnZiK3LEbQeBwe"],"assetId":"5TBkQTEnyN8qYvfnDMnVQss3DuRVLmXSnyFFLS5AnzvE","transfers":[],"id":"CwJMbjdXchdqupzCkoALGBzg5Zp72duzhGfURLR5iwK1","timestamp":215734165446088575}"""
        )
      )
      .explicitGet()

    val parsed = TransactionParsers.parseBytes(transaction.bytes())
    parsed should matchPattern {
      case Success(mtt: MassTransferTransaction) if mtt.transfers.isEmpty => // Success
    }
  }
}
