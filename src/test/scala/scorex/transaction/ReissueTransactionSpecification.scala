package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.{IssueTransaction, ReissueTransaction}

import scala.util.Try

class ReissueTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {


  def parseBytes(bytes: Array[Byte]): Try[ReissueTransaction] = Try {
    require(bytes.head == TransactionType.ReissueTransaction.id)
    ReissueTransaction.parseTail(bytes.tail).get
  }

  property("Reissue serialization roundtrip") {
    forAll(reissueGenerator) { issue: ReissueTransaction =>
      val recovered = parseBytes(issue.bytes).get
      recovered.bytes shouldEqual issue.bytes
    }
  }

  property("Reissue serialization from TypedTransaction") {
    forAll(reissueGenerator) { issue: ReissueTransaction =>
      val recovered = TransactionParser.parseBytes(issue.bytes).get
      recovered.bytes shouldEqual issue.bytes
    }
  }

}
