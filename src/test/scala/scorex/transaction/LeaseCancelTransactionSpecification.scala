package scorex.transaction

import com.wavesplatform.OldTransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.lease.LeaseCancelTransaction

class LeaseCancelTransactionSpecification extends PropSpec with PropertyChecks with Matchers with OldTransactionGen {

  property("Lease cancel serialization roundtrip") {
    forAll(leaseCancelGen) { tx: LeaseCancelTransaction =>
      val recovered = LeaseCancelTransaction.parseBytes(tx.bytes()).get
      assertTxs(recovered, tx)
    }
  }

  property("Lease cancel serialization from TypedTransaction") {
    forAll(leaseCancelGen) { tx: LeaseCancelTransaction =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[LeaseCancelTransaction], tx)
    }
  }

  private def assertTxs(first: LeaseCancelTransaction, second: LeaseCancelTransaction): Unit = {
    first.leaseId shouldEqual second.leaseId
    first.fee shouldEqual second.fee
    first.signature shouldEqual second.signature
    first.bytes() shouldEqual second.bytes()
  }

}
