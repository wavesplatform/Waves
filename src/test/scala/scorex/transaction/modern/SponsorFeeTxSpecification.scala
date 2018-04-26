package scorex.transaction.modern

import com.wavesplatform.ModernTransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.modern.assets.SponsorFeeTx
import scorex.transaction.TransactionParsers

class SponsorFeeTxSpecification extends PropSpec with PropertyChecks with Matchers with ModernTransactionGen {

  property("SponsorFee serialization roundtrip") {
    forAll(sponsorFeeTxGen) { transaction: SponsorFeeTx =>
      val recovered = SponsorFeeTx.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

  property("SponsorFee serialization from TypedTransaction") {
    forAll(sponsorFeeTxGen) { transaction: SponsorFeeTx =>
      val recovered = TransactionParsers.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

}
