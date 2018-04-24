package scorex.transaction.modern.smart

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.{ModernTransactionGen, TransactionParsers}

class SetScriptTxSpecification extends PropSpec with PropertyChecks with Matchers with ModernTransactionGen {

  property("SetScriptTx serialization roudtrip") {
    forAll(setScriptTxGen) { tx =>
      val recovered = SetScriptTx.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("SetScriptTx serialization from TypedTransaction") {
    forAll(setScriptTxGen) { tx =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

}
