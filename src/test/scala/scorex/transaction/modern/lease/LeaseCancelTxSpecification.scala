package scorex.transaction.modern.lease

import com.wavesplatform.ModernTransactionGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.TransactionParsers

class LeaseCancelTxSpecification extends PropSpec with PropertyChecks with Matchers with ModernTransactionGen {

  property("LeaseCancelTx serialization roudtrip") {
    forAll(leaseCancelTxGen) { tx =>
      val recovered = LeaseCancelTx.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("LeaseCancelTx serialization from TypedTransaction") {
    forAll(leaseCancelTxGen) { tx =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

}
