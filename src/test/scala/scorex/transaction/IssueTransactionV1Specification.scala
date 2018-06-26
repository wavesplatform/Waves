package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.IssueTransaction

class IssueTransactionV1Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Issue serialization roundtrip") {
    forAll(issueGen) { issue: IssueTransaction =>
      val recovered = issue.builder.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("Issue serialization from TypedTransaction") {
    forAll(issueGen) { issue: IssueTransaction =>
      val recovered = TransactionParsers.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

}
