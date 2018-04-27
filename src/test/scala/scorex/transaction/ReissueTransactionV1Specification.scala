package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.ReissueTransactionV1

class ReissueTransactionV1Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Reissue serialization roundtrip") {
    forAll(reissueGen) { issue: ReissueTransactionV1 =>
      val recovered = ReissueTransactionV1.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("Reissue serialization from TypedTransaction") {
    forAll(reissueGen) { issue: ReissueTransactionV1 =>
      val recovered = TransactionParsers.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

}
