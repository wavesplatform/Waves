package scorex.transaction.modern.assets

import com.wavesplatform.ModernTransactionGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.TransactionParsers

class TransferTxSpecification extends PropSpec with PropertyChecks with Matchers with ModernTransactionGen {

  property("TransferTx serialization roudtrip") {
    forAll(transferTxGen) { tx =>
      val recovered = TransferTx.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("TransferTx serialization from TypedTransaction") {
    forAll(transferTxGen) { tx =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

}
