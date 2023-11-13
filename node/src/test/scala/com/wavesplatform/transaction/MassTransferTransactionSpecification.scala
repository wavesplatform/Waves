package com.wavesplatform.transaction

import com.wavesplatform.account.{AddressScheme, Alias, KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.serialization.impl.{MassTransferTxSerializer, PBTransactionSerializer}
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.transfer.MassTransferTransaction.{MaxTransferCount, ParsedTransfer, Transfer}
import play.api.libs.json.Json

import java.nio.charset.StandardCharsets
import scala.util.{Random, Success}

class MassTransferTransactionSpecification extends PropSpec {

  private val massTransferTxSupportedVersions: Seq[Byte] = Seq(1, 2)

  private val sender    = KeyPair("sender".getBytes(StandardCharsets.UTF_8))
  private val recipient = KeyPair("recipient".getBytes(StandardCharsets.UTF_8))

  private val asset = IssuedAsset(ByteStr((1 to AssetIdLength).map(_.toByte).to(Array)))

  private val proofs = Seq(
    Seq.empty,
    Seq(ByteStr.empty),
    Seq(ByteStr(Random.nextBytes(Proofs.MaxProofSize))),
    (1 to Proofs.MaxProofs).map(_ => ByteStr.empty),
    (1 to Proofs.MaxProofs).map(_ => ByteStr(Random.nextBytes(Proofs.MaxProofSize)))
  )

  private val massTransfers = for {
    chainId   <- Seq(Byte.MinValue, 0: Byte, AddressScheme.current.chainId, Byte.MaxValue)
    version   <- massTransferTxSupportedVersions
    asset     <- Seq(Waves, asset)
    recipient <- Seq(recipient.toAddress(chainId), Alias(chainId, "0000"))
    fee       <- Seq(1, Long.MaxValue)
    transfers <- Seq(
      Seq.empty,
      Seq(ParsedTransfer(recipient, TxNonNegativeAmount.unsafeFrom(Long.MaxValue - fee))),
      (1 to MaxTransferCount).map(_ => ParsedTransfer(recipient, TxNonNegativeAmount.unsafeFrom(1 / MaxTransferCount)))
    )
    attachment <- Seq(ByteStr.empty, ByteStr(Random.nextBytes(TransferTransaction.MaxAttachmentSize)))
    proofs     <- proofs
  } yield (chainId, version, asset, recipient, fee, transfers, attachment, proofs)

  private val massTransfersTable = Table(
    ("chainId", "version", "asset", "recipient", "fee", "transfers", "attachment", "proofs"),
    massTransfers*
  )

  property("serialization roundtrip") {
    forAll(massTransfersTable) { case (chainId, version, asset, _, fee, transfers, attachment, proofs) =>
      val tx = MassTransferTransaction(
        version = version,
        sender = sender.publicKey,
        assetId = asset,
        transfers = transfers,
        fee = TxPositiveAmount.unsafeFrom(fee),
        timestamp = 1L,
        attachment = attachment,
        proofs = Proofs(proofs),
        chainId = chainId
      )
      val bytes = tx.bytes()
      val recovered = {
        if (PBSince.affects(tx)) PBTransactionSerializer.parseBytes(bytes)
        else MassTransferTransaction.parseBytes(bytes)
      }.get
      recovered shouldEqual tx
      recovered.bytes() shouldEqual bytes
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

    val tx = MassTransferTxSerializer.parseBytes(bytes).get
    tx.json() shouldBe json
    assert(crypto.verify(tx.signature, tx.bodyBytes(), tx.sender), "signature should be valid")
  }

  property("serialization from TypedTransaction") {
    forAll(massTransfersTable) { case (chainId, version, asset, _, fee, transfers, attachment, proofs) =>
      val tx = MassTransferTransaction(
        version = version,
        sender = sender.publicKey,
        assetId = asset,
        transfers = transfers,
        fee = TxPositiveAmount.unsafeFrom(fee),
        timestamp = 1L,
        attachment = attachment,
        proofs = Proofs(proofs),
        chainId = chainId
      )
      if (!PBSince.affects(tx) && tx.chainId == AddressScheme.current.chainId) {
        val recovered = TransactionParsers.parseBytes(tx.bytes()).get
        recovered.bytes() shouldEqual tx.bytes()
      }
    }
  }

  property("property validation") {
    import MassTransferTransaction.create

    val timestamp = 1L

    val (_, _, assetId, _, fee, transfers, attachment, proofs) = massTransfers.head

    val tooManyTransfers   = List.fill(MaxTransferCount + 1)(ParsedTransfer(sender.toAddress, TxNonNegativeAmount.unsafeFrom(1L)))
    val tooManyTransfersEi = create(1.toByte, sender.publicKey, asset, tooManyTransfers, fee, timestamp, attachment, proofs)
    tooManyTransfersEi shouldBe Left(GenericError(s"Number of transfers ${tooManyTransfers.length} is greater than $MaxTransferCount"))

    val oneHalf    = Long.MaxValue / 2 + 1
    val overflow   = List.fill(2)(ParsedTransfer(sender.toAddress, TxNonNegativeAmount.unsafeFrom(oneHalf)))
    val overflowEi = create(1.toByte, sender.publicKey, assetId, overflow, fee, timestamp, attachment, proofs)
    overflowEi shouldBe Left(TxValidationError.OverflowError)

    val feeOverflow   = List(ParsedTransfer(sender.toAddress, TxNonNegativeAmount.unsafeFrom(oneHalf)))
    val feeOverflowEi = create(1.toByte, sender.publicKey, assetId, feeOverflow, oneHalf, timestamp, attachment, proofs)
    feeOverflowEi shouldBe Left(TxValidationError.OverflowError)

    val longAttachment   = ByteStr(Array.fill(TransferTransaction.MaxAttachmentSize + 1)(1: Byte))
    val longAttachmentEi = create(1.toByte, sender.publicKey, assetId, transfers, fee, timestamp, longAttachment, proofs)
    longAttachmentEi shouldBe Left(
      TxValidationError.TooBigInBytes(
        s"Invalid attachment. Length ${TransferTransaction.MaxAttachmentSize + 1} bytes exceeds maximum of ${TransferTransaction.MaxAttachmentSize} bytes."
      )
    )

    val noFeeEi = create(1.toByte, sender.publicKey, assetId, feeOverflow, 0, timestamp, attachment, proofs)
    noFeeEi shouldBe Left(TxValidationError.InsufficientFee)

    val negativeFeeEi = create(1.toByte, sender.publicKey, assetId, feeOverflow, -100, timestamp, attachment, proofs)
    negativeFeeEi shouldBe Left(TxValidationError.InsufficientFee)

    val differentChainIds = Seq(
      ParsedTransfer(sender.toAddress, TxNonNegativeAmount.unsafeFrom(100)),
      ParsedTransfer(sender.toAddress('?'.toByte), TxNonNegativeAmount.unsafeFrom(100))
    )
    val invalidChainIdEi = create(1.toByte, sender.publicKey, assetId, differentChainIds, 100, timestamp, attachment, proofs)
    invalidChainIdEi should produce("One of chain ids not match")

    val otherChainIds = Seq(
      ParsedTransfer(sender.toAddress('?'.toByte), TxNonNegativeAmount.unsafeFrom(100)),
      ParsedTransfer(sender.toAddress('?'.toByte), TxNonNegativeAmount.unsafeFrom(100))
    )
    val invalidOtherChainIdEi = create(1.toByte, sender.publicKey, assetId, otherChainIds, 100, timestamp, attachment, proofs)
    invalidOtherChainIdEi should produce("One of chain ids not match")
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
      .explicitGet()

    val tx = MassTransferTransaction
      .create(
        1.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Waves,
        transfers,
        200000,
        1518091313964L,
        ByteStr.decodeBase58("59QuUcqP6p").get,
        Proofs(Seq(ByteStr.decodeBase58("FXMNu3ecy5zBjn9b69VtpuYRwxjCbxdkZ3xZpLzB8ZeFDvcgTkmEDrD29wtGYRPtyLS3LPYrL2d5UM6TpFBMUGQ").get))
      )
      .explicitGet()

    js shouldEqual tx.json()
  }

  property("empty transfers validation") {
    import play.api.libs.json.*
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
