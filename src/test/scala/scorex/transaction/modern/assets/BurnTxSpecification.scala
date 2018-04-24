package scorex.transaction.modern.assets

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.{ModernTransactionGen, TransactionParsers}

class BurnTxSpecification extends PropSpec with PropertyChecks with Matchers with ModernTransactionGen {

  property("BurnTx serialization roudtrip") {
    forAll(burnTxGen) { tx =>
      val recovered = BurnTx.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("BurnTx serialization from TypedTransaction") {
    forAll(burnTxGen) { tx =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }
}
