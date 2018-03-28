package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.ReissueTransaction

class ReissueTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Reissue serialization roundtrip") {
    forAll(reissueGen) { issue: ReissueTransaction =>
      val recovered = ReissueTransaction.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("Reissue serialization from TypedTransaction") {
    forAll(reissueGen) { issue: ReissueTransaction =>
      val recovered = TransactionParsers.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

}
