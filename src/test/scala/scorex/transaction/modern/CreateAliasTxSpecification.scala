package scorex.transaction.modern

import com.wavesplatform.ModernTransactionGen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scorex.transaction.TransactionParsers

class CreateAliasTxSpecification extends PropSpec with PropertyChecks with Matchers with ModernTransactionGen {
  property("CreateAliasTx serialization roudtrip") {
    forAll(createAliasTxGen) { tx =>
      val recovered = CreateAliasTx.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("CreateAliasTx serialization from TypedTransaction") {
    forAll(createAliasTxGen) { tx =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }
}
