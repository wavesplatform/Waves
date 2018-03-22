package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.MassTransferTransaction.{MaxTransferCount, ParsedTransfer}
import scorex.transaction.assets.{MassTransferTransaction, TransferTransaction}

class MassTransferTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("serialization roundtrip") {
    forAll(massTransferGen) { tx: MassTransferTransaction =>
      require(tx.bytes().head == TransactionType.MassTransferTransaction.id)
      val recovered = MassTransferTransaction.parseTail(tx.bytes().tail).get

      recovered.sender.address shouldEqual tx.sender.address
      recovered.assetId.map(_ == tx.assetId.get).getOrElse(tx.assetId.isEmpty) shouldBe true
      recovered.timestamp shouldEqual tx.timestamp
      recovered.fee shouldEqual tx.fee

      recovered.transfers.zip(tx.transfers).foreach { case (ParsedTransfer(rr, ra), ParsedTransfer(tr, ta)) =>
          rr shouldEqual tr
          ra shouldEqual ta
      }

      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("serialization from TypedTransaction") {
    forAll(massTransferGen) { tx: MassTransferTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("property validation") {
    import MassTransferTransaction.create

    val badVersionGen = Gen.choose(MassTransferTransaction.Version + 1, Byte.MaxValue).map(_.toByte)
    forAll(massTransferGen, badVersionGen) {
      case (MassTransferTransaction(version, assetId, sender, transfers, timestamp, fee, attachment, proofs), badVersion) =>
        val badVersionEi = create(badVersion, assetId, sender, transfers, timestamp, fee, attachment, proofs)
        badVersionEi shouldBe Left(ValidationError.UnsupportedVersion(badVersion))

        val tooManyTransfers = List.fill(MaxTransferCount + 1)(ParsedTransfer(sender.toAddress, 1L))
        val tooManyTransfersEi = create(version, assetId, sender, tooManyTransfers, timestamp, fee, attachment, proofs)
        tooManyTransfersEi shouldBe Left(GenericError(s"Number of transfers is greater than $MaxTransferCount"))

        val negativeTransfer = List(ParsedTransfer(sender.toAddress, -1L))
        val negativeTransferEi = create(version, assetId, sender, negativeTransfer, timestamp, fee, attachment, proofs)
        negativeTransferEi shouldBe Left(GenericError("One of the transfers has negative amount"))

        val oneHalf = Long.MaxValue / 2 + 1
        val overflow = List.fill(2)(ParsedTransfer(sender.toAddress, oneHalf))
        val overflowEi = create(version, assetId, sender, overflow, timestamp, fee, attachment, proofs)
        overflowEi shouldBe Left(ValidationError.OverflowError)

        val feeOverflow = List(ParsedTransfer(sender.toAddress, oneHalf))
        val feeOverflowEi = create(version, assetId, sender, feeOverflow, timestamp, oneHalf, attachment, proofs)
        feeOverflowEi shouldBe Left(ValidationError.OverflowError)

        val longAttachment = Array.fill(TransferTransaction.MaxAttachmentSize + 1)(1: Byte)
        val longAttachmentEi = create(version, assetId, sender, transfers, timestamp, fee, longAttachment, proofs)
        longAttachmentEi shouldBe Left(ValidationError.TooBigArray)

        val noFeeEi = create(version, assetId, sender, feeOverflow, timestamp, 0, attachment, proofs)
        noFeeEi shouldBe Left(ValidationError.InsufficientFee)

        val negativeFeeEi = create(version, assetId, sender, feeOverflow, timestamp, -100, attachment, proofs)
        negativeFeeEi shouldBe Left(ValidationError.InsufficientFee)
    }
  }
}
