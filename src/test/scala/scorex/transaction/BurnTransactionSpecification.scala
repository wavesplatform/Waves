package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.BurnTransaction

class BurnTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Burn serialization roundtrip") {
    forAll(burnGenerator) { issue: BurnTransaction =>
      val recovered = BurnTransaction.parseBytes(issue.bytes).get
      recovered.bytes shouldEqual issue.bytes
    }
  }

  property("Burn serialization from TypedTransaction") {
    forAll(burnGenerator) { issue: BurnTransaction =>
      val recovered = TypedTransaction.parseBytes(issue.bytes).get
      recovered.bytes shouldEqual issue.bytes
    }
  }

}
