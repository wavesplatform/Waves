package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.IssueTransactionV1

class IssueTransactionV1Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Issue serialization roundtrip") {
    forAll(issueGen) { issue: IssueTransactionV1 =>
      val recovered = IssueTransactionV1.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("Issue serialization from TypedTransaction") {
    forAll(issueGen) { issue: IssueTransactionV1 =>
      val recovered = TransactionParsers.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

}
