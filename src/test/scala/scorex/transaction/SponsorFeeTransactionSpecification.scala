package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.SponsorFeeTransaction

class SponsorFeeTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("SponsorFee serialization roundtrip") {
    forAll(sponsorFeeGen) { transaction: SponsorFeeTransaction =>
      val recovered = SponsorFeeTransaction.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

  property("SponsorFee serialization from TypedTransaction") {
    forAll(sponsorFeeGen) { transaction: SponsorFeeTransaction =>
      val recovered = TransactionParsers.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

}
