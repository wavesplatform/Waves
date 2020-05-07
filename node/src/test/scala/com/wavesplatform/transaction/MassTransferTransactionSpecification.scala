package com.wavesplatform.transaction

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.state.diffs.ProduceError._
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.transfer.MassTransferTransaction.{MaxTransferCount, ParsedTransfer, Transfer}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.{TransactionGen, crypto}
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.Json

import scala.util.Success

class MassTransferTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("serialization roundtrip") {
    forAll(massTransferGen) { tx: MassTransferTransaction =>
      require(tx.bytes().head == MassTransferTransaction.typeId)
      val recovered = MassTransferTransaction.parseBytes(tx.bytes()).get

      recovered.sender shouldEqual tx.sender
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

  property("decode pre-encoded bytes") {
    val bytes = Base64.decode(
      "CwHVKKq+w1yhANh8e3oShjL68ZzURTGBlFdEUROjKiHvIgAAAgFUqGL1rZ+cUjoGcrHCi5yhcMFfaIJfkO4AAAAAAAX14QABVKhi9a2fnFI6BnKxwoucoXDBX2iCX5DuAAAAAAAL68IAAAABYXVLIywAAAAAAAMNQAAHbWFzc3BheQEAAQBADIY7QdjAPaDZwHpkXBIEd7XQZE/E7ihi//v3RizdqW2alpM0DWJJ6PcyLOOcYbeBvLJx49Xv2uCTgIMIEIiyiQ=="
    )
    val json = Json.parse("""{
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

    val tx = MassTransferTransaction.serializer.parseBytes(bytes).get
    tx.json() shouldBe json
    assert(crypto.verify(tx.signature, tx.bodyBytes(), tx.sender), "signature should be valid")
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
      case MassTransferTransaction(_, sender, assetId, transfers, fee, timestamp, attachment, proofs,_ ) =>
        val tooManyTransfers   = List.fill(MaxTransferCount + 1)(ParsedTransfer(sender.toAddress, 1L))
        val tooManyTransfersEi = create(1.toByte, sender, assetId, tooManyTransfers, fee, timestamp, attachment, proofs)
        tooManyTransfersEi shouldBe Left(GenericError(s"Number of transfers ${tooManyTransfers.length} is greater than $MaxTransferCount"))

        val negativeTransfer   = List(ParsedTransfer(sender.toAddress, -1L))
        val negativeTransferEi = create(1.toByte, sender, assetId, negativeTransfer, fee, timestamp, attachment, proofs)
        negativeTransferEi shouldBe Left(GenericError("One of the transfers has negative amount"))

        val oneHalf    = Long.MaxValue / 2 + 1
        val overflow   = List.fill(2)(ParsedTransfer(sender.toAddress, oneHalf))
        val overflowEi = create(1.toByte, sender, assetId, overflow, fee, timestamp, attachment, proofs)
        overflowEi shouldBe Left(TxValidationError.OverflowError)

        val feeOverflow   = List(ParsedTransfer(sender.toAddress, oneHalf))
        val feeOverflowEi = create(1.toByte, sender, assetId, feeOverflow, oneHalf, timestamp, attachment, proofs)
        feeOverflowEi shouldBe Left(TxValidationError.OverflowError)

        val longAttachment   = Attachment.Bin(Array.fill(TransferTransaction.MaxAttachmentSize + 1)(1: Byte))
        val longAttachmentEi = create(1.toByte, sender, assetId, transfers, fee, timestamp, Some(longAttachment), proofs)
        longAttachmentEi shouldBe Left(TxValidationError.TooBigArray)

        val noFeeEi = create(1.toByte, sender, assetId, feeOverflow, 0, timestamp, attachment, proofs)
        noFeeEi shouldBe Left(TxValidationError.InsufficientFee())

        val negativeFeeEi = create(1.toByte, sender, assetId, feeOverflow, -100, timestamp, attachment, proofs)
        negativeFeeEi shouldBe Left(TxValidationError.InsufficientFee())

        val differentChainIds = Seq(ParsedTransfer(sender.toAddress, 100), ParsedTransfer(sender.toAddress('?'.toByte), 100))
        val invalidChainIdEi = create(1.toByte, sender, assetId, differentChainIds, 100, timestamp, attachment, proofs)
        invalidChainIdEi should produce("One of chain ids not match")

        val otherChainIds = Seq(ParsedTransfer(sender.toAddress('?'.toByte), 100), ParsedTransfer(sender.toAddress('?'.toByte), 100))
        val invalidOtherChainIdEi = create(1.toByte, sender, assetId, otherChainIds, 100, timestamp, attachment, proofs)
        invalidOtherChainIdEi should produce("One of chain ids not match")
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
        1.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Waves,
        transfers,
        200000,
        1518091313964L,
        Some(Attachment.Bin(Base58.tryDecodeWithLimit("59QuUcqP6p").get)),
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
