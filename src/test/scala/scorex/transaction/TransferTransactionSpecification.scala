package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.TransferTransaction

class TransferTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Transfer serialization roundtrip") {
    forAll(transferGenerator) { transfer: TransferTransaction =>
      val recovered = TransferTransaction.parseBytes(transfer.bytes).get

      recovered.sender.address shouldEqual transfer.sender.address
      recovered.assetId.map(_ sameElements transfer.assetId.get).getOrElse(transfer.assetId.isEmpty) shouldBe true
      recovered.feeAssetId.map(_ sameElements transfer.feeAssetId.get).getOrElse(transfer.feeAssetId.isEmpty) shouldBe true
      recovered.timestamp shouldEqual transfer.timestamp
      recovered.amount shouldEqual transfer.amount
      recovered.fee shouldEqual transfer.fee
      recovered.recipient.address shouldEqual transfer.recipient.address

      recovered.bytes shouldEqual transfer.bytes
    }
  }

  property("Transfer serialization from TypedTransaction") {
    forAll(transferGenerator) { tx: TransferTransaction =>
      val recovered = TypedTransaction.parseBytes(tx.bytes).get
      recovered.bytes shouldEqual tx.bytes
    }
  }

}
