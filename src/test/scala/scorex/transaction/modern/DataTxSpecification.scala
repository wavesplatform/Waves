package scorex.transaction.modern

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scorex.transaction.{ModernTransactionGen, TransactionParsers}

class DataTxSpecification extends PropSpec with PropertyChecks with Matchers with ModernTransactionGen {
  property("DataTx serialization roudtrip") {
    forAll(dataTxGen) { tx =>
      val recovered = DataTx.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("DataTx serialization from TypedTransaction") {
    forAll(dataTxGen) { tx =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }
}
