package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.BurnTransaction

class BurnTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Burn serialization roundtrip") {
    forAll(burnGen) { issue: BurnTransaction =>
      val recovered = BurnTransaction.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("Burn serialization from TypedTransaction") {
    forAll(burnGen) { issue: BurnTransaction =>
      val recovered = TransactionParser.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

}
