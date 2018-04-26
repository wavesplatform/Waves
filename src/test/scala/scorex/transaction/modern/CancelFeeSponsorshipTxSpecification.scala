package scorex.transaction.modern

import com.wavesplatform.ModernTransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.modern.assets.CancelFeeSponsorshipTx
import scorex.transaction.TransactionParsers

class CancelFeeSponsorshipTxSpecification extends PropSpec with PropertyChecks with Matchers with ModernTransactionGen {

  property("SponsorFee serialization roundtrip") {
    forAll(cancelFeeSponsorshipTxGen) { transaction: CancelFeeSponsorshipTx =>
      val recovered = CancelFeeSponsorshipTx.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

  property("SponsorFee serialization from TypedTransaction") {
    forAll(cancelFeeSponsorshipTxGen) { transaction: CancelFeeSponsorshipTx =>
      val recovered = TransactionParsers.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

}
