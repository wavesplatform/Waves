package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.PublicKeyAccount
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.assets.exchange.Order
import scorex.utils.NTP

class IssueTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Issue transaction serialization roundtrip") {
    forAll(issueGenerator) { issue: IssueTransaction =>
      val recovered = Order.parseBytes(issue.bytes).get
      recovered.bytes shouldEqual issue.bytes
    }
  }
}
