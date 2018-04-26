package scorex.transaction.modern.assets

import com.wavesplatform.ModernTransactionGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.TransactionParsers

class IssueTxSpecification extends PropSpec with PropertyChecks with Matchers with ModernTransactionGen {

  property("IssueTx serialization roudtrip") {
    forAll(issueTxGen) { tx =>
      val recovered = IssueTx.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("IssueTx serialization from TypedTransaction") {
    forAll(issueTxGen) { tx =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

}
