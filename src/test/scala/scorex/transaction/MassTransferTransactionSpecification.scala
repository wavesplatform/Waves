package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.MassTransferTransaction.MaxTransferCount
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

      recovered.transfers.zip(tx.transfers).foreach { case ((rr, ra), (tr, ta)) =>
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
    forAll(massTransferGen) {
      case MassTransferTransaction(assetId, sender, transfers, timestamp, fee, attachment, signature) =>
        val tooManyTransfers = List.fill(MaxTransferCount + 1)((sender.toAddress, 1L))
        val tooManyTransfersEi = create(assetId, sender, tooManyTransfers, timestamp, fee, attachment, signature)
        tooManyTransfersEi shouldBe Left(GenericError(s"Number of transfers is greater than $MaxTransferCount"))

        val negativeTransfer = List((sender.toAddress, -1L))
        val negativeTransferEi = create(assetId, sender, negativeTransfer, timestamp, fee, attachment, signature)
        negativeTransferEi shouldBe Left(GenericError("One of the transfers has negative amount"))

        val oneHalf = Long.MaxValue / 2 + 1
        val overflow = List.fill(2)((sender.toAddress, oneHalf))
        val overflowEi = create(assetId, sender, overflow, timestamp, fee, attachment, signature)
        overflowEi shouldBe Left(ValidationError.OverflowError)

        val feeOverflow = List((sender.toAddress, oneHalf))
        val feeOverflowEi = create(assetId, sender, feeOverflow, timestamp, oneHalf, attachment, signature)
        feeOverflowEi shouldBe Left(ValidationError.OverflowError)

        val longAttachment = Array.fill(TransferTransaction.MaxAttachmentSize + 1)(1: Byte)
        val longAttachmentEi = create(assetId, sender, transfers, timestamp, fee, longAttachment, signature)
        longAttachmentEi shouldBe Left(ValidationError.TooBigArray)

        val noFeeEi = create(assetId, sender, feeOverflow, timestamp, 0, attachment, signature)
        noFeeEi shouldBe Left(ValidationError.InsufficientFee)

        val negativeFeeEi = create(assetId, sender, feeOverflow, timestamp, -100, attachment, signature)
        negativeFeeEi shouldBe Left(ValidationError.InsufficientFee)
    }
  }
}
