package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.V1TransferTransaction

class V1TransferTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Transfer serialization roundtrip") {
    forAll(transferGen) { transfer: V1TransferTransaction =>
      val recovered = V1TransferTransaction.parseBytes(transfer.bytes()).get

      recovered.sender.address shouldEqual transfer.sender.address
      recovered.assetId.map(_ == transfer.assetId.get).getOrElse(transfer.assetId.isEmpty) shouldBe true
      recovered.feeAssetId.map(_ == transfer.feeAssetId.get).getOrElse(transfer.feeAssetId.isEmpty) shouldBe true
      recovered.timestamp shouldEqual transfer.timestamp
      recovered.amount shouldEqual transfer.amount
      recovered.fee shouldEqual transfer.fee
      recovered.recipient.stringRepr shouldEqual transfer.recipient.stringRepr

      recovered.bytes() shouldEqual transfer.bytes()
    }
  }

  property("Transfer serialization from TypedTransaction") {
    forAll(transferGen) { tx: V1TransferTransaction =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

}
