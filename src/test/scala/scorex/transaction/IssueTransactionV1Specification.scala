package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.{IssueTransaction, IssueTransactionV1}

class IssueTransactionV1Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Issue serialization roundtrip") {
    forAll(issueV1Gen) { issue: IssueTransaction =>
      val recovered = IssueTransactionV1.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("Issue serialization from TypedTransaction") {
    forAll(issueV1Gen) { issue: IssueTransaction =>
      val recovered = TransactionParsers.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

}
