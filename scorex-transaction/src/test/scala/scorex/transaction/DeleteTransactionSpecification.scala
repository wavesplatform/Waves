package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.{DeleteTransaction, ReissueTransaction}

class DeleteTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Delete serialization roundtrip") {
    forAll(deleteGenerator) { issue: DeleteTransaction =>
      val recovered = DeleteTransaction.parseBytes(issue.bytes).get
      recovered.bytes shouldEqual issue.bytes
    }
  }

  property("Delete serialization from TypedTransaction") {
    forAll(deleteGenerator) { issue: DeleteTransaction =>
      val recovered = TypedTransaction.parseBytes(issue.bytes).get
      recovered.bytes shouldEqual issue.bytes
    }
  }

}
