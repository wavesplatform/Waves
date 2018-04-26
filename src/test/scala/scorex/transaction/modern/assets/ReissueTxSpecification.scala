package scorex.transaction.modern.assets

import com.wavesplatform.ModernTransactionGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.TransactionParsers

class ReissueTxSpecification extends PropSpec with PropertyChecks with Matchers with ModernTransactionGen {

  property("ReissueTx serialization roudtrip") {
    forAll(reissueTxGen) { tx =>
      val recovered = ReissueTx.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("ReissueTx serialization from TypedTransaction") {
    forAll(reissueTxGen) { tx =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

}
