package scorex.transaction.modern.lease

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.{ModernTransactionGen, TransactionParsers}

class LeaseTxSpecification extends PropSpec with PropertyChecks with Matchers with ModernTransactionGen  {

  property("LeaseTx serialization roudtrip") {
    forAll(leaseTxGen) { tx =>
      val recovered = LeaseTx.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("LeaseTx serialization from TypedTransaction") {
    forAll(leaseTxGen) { tx =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

}
