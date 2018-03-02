package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{Format, Json, Writes}
import scorex.api.http.SignedDataRequest
import scorex.transaction.DataTransaction.ParsedItem
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.MassTransferTransaction.{MaxTransferCount, ParsedTransfer}
import scorex.transaction.assets.{MassTransferTransaction, TransferTransaction}

class DataTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("serialization roundtrip") {
    forAll(dataTransactionGen) { tx: DataTransaction =>
      require(tx.bytes().head == TransactionType.DataTransaction.id)
      val recovered = DataTransaction.parseTail(tx.bytes().tail).get

      recovered.sender.address shouldEqual tx.sender.address
      recovered.timestamp shouldEqual tx.timestamp
      recovered.fee shouldEqual tx.fee

      recovered.data.zip(tx.data).foreach { case (ParsedItem(rk, rv), ParsedItem(tk, tv)) =>
        rk shouldEqual tk
        rv shouldEqual tv
      }

      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("serialization from TypedTransaction") {
    forAll(dataTransactionGen) { tx: DataTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {///needed?
    import scorex.api.http.DataRequest.signedFormat
//    import DataTransaction.itemFormat
//
//    implicit val dataTransactionFormat: Format[DataTransaction] = Json.format

    forAll(dataTransactionGen) { tx0: DataTransaction =>
      val json0 = tx0.json()
//      val tx1 = json0.as[DataTransaction]
//      val json1 = tx1.json()
      tx0.toString shouldEqual json0.toString

      val req = json0.as[SignedDataRequest]
      Console.err.println("json " + json0)
      Console.err.println("req " + req)

    }
  }

//  property("property validation") {
//    import MassTransferTransaction.create
//    forAll(massTransferGen) {
//      case MassTransferTransaction(version, assetId, sender, transfers, timestamp, fee, attachment, proofs) =>
//        val tooManyTransfers = List.fill(MaxTransferCount + 1)(ParsedTransfer(sender.toAddress, 1L))
//        val tooManyTransfersEi = create(version, assetId, sender, tooManyTransfers, timestamp, fee, attachment, proofs)
//        tooManyTransfersEi shouldBe Left(GenericError(s"Number of transfers is greater than $MaxTransferCount"))
//
//        val negativeTransfer = List(ParsedTransfer(sender.toAddress, -1L))
//        val negativeTransferEi = create(version, assetId, sender, negativeTransfer, timestamp, fee, attachment, proofs)
//        negativeTransferEi shouldBe Left(GenericError("One of the transfers has negative amount"))
//
//        val oneHalf = Long.MaxValue / 2 + 1
//        val overflow = List.fill(2)(ParsedTransfer(sender.toAddress, oneHalf))
//        val overflowEi = create(version, assetId, sender, overflow, timestamp, fee, attachment, proofs)
//        overflowEi shouldBe Left(ValidationError.OverflowError)
//
//        val feeOverflow = List(ParsedTransfer(sender.toAddress, oneHalf))
//        val feeOverflowEi = create(version, assetId, sender, feeOverflow, timestamp, oneHalf, attachment, proofs)
//        feeOverflowEi shouldBe Left(ValidationError.OverflowError)
//
//        val longAttachment = Array.fill(TransferTransaction.MaxAttachmentSize + 1)(1: Byte)
//        val longAttachmentEi = create(version, assetId, sender, transfers, timestamp, fee, longAttachment, proofs)
//        longAttachmentEi shouldBe Left(ValidationError.TooBigArray)
//
//        val noFeeEi = create(version, assetId, sender, feeOverflow, timestamp, 0, attachment, proofs)
//        noFeeEi shouldBe Left(ValidationError.InsufficientFee)
//
//        val negativeFeeEi = create(version, assetId, sender, feeOverflow, timestamp, -100, attachment, proofs)
//        negativeFeeEi shouldBe Left(ValidationError.InsufficientFee)
//    }
//  }
}
