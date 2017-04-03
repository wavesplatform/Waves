package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.TransferTransaction

class TransferTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Transfer serialization roundtrip") {
    forAll(transferGenerator) { transfer: TransferTransaction =>
      require(transfer.bytes.head == TransactionType.TransferTransaction.id)
      val recovered = TransferTransaction.parseTail(transfer.bytes.tail).get

      recovered.sender.address shouldEqual transfer.sender.address
      recovered.assetId.map(_ sameElements transfer.assetId.get).getOrElse(transfer.assetId.isEmpty) shouldBe true
      recovered.feeAssetId.map(_ sameElements transfer.feeAssetId.get).getOrElse(transfer.feeAssetId.isEmpty) shouldBe true
      recovered.timestamp shouldEqual transfer.timestamp
      recovered.amount shouldEqual transfer.amount
      recovered.fee shouldEqual transfer.fee
      recovered.recipient.stringRepr shouldEqual transfer.recipient.stringRepr

      recovered.bytes shouldEqual transfer.bytes
    }
  }

  property("Transfer serialization from TypedTransaction") {
    forAll(transferGenerator) { tx: TransferTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes).get
      recovered.bytes shouldEqual tx.bytes
    }
  }

}
