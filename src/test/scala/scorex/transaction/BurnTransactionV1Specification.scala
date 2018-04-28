package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.BurnTransactionV1

class BurnTransactionV1Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Burn serialization roundtrip") {
    forAll(burnGen) { issue: BurnTransactionV1 =>
      val recovered = BurnTransactionV1.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("Burn serialization from TypedTransaction") {
    forAll(burnGen) { issue: BurnTransactionV1 =>
      val recovered = TransactionParsers.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

}
