package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.{ReissueTransaction, IssueTransaction}

class ReissueTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Reissue serialization roundtrip") {
    forAll(reissueGenerator) { issue: ReissueTransaction =>
      val recovered = ReissueTransaction.parseBytes(issue.bytes).get
      recovered.bytes shouldEqual issue.bytes
    }
  }

  property("Reissue serialization from TypedTransaction") {
    forAll(reissueGenerator) { issue: ReissueTransaction =>
      val recovered = TypedTransaction.parseBytes(issue.bytes).get
      recovered.bytes shouldEqual issue.bytes
    }
  }

}
