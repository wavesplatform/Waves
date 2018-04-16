package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.CancelFeeSponsorshipTransaction

class CancelFeeSponsorshipTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("SponsorFee serialization roundtrip") {
    forAll(cancelFeeSponsorshipGen) { transaction: CancelFeeSponsorshipTransaction =>
      val recovered = CancelFeeSponsorshipTransaction.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

  property("SponsorFee serialization from TypedTransaction") {
    forAll(cancelFeeSponsorshipGen) { transaction: CancelFeeSponsorshipTransaction =>
      val recovered = TransactionParsers.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

}
