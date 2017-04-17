package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.lease.LeaseCancelTransaction

class LeaseCancelTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Lease cancel serialization roundtrip") {
    forAll(leaseCancelGenerator) { tx: LeaseCancelTransaction =>
      val recovered = LeaseCancelTransaction.parseBytes(tx.bytes).get

      assertTxs(recovered, tx)
    }
  }

  property("Lease cancel serialization from TypedTransaction") {
    forAll(leaseCancelGenerator) { tx: LeaseCancelTransaction =>
      val recovered = TypedTransaction.parseBytes(tx.bytes).get

      assertTxs(recovered.asInstanceOf[LeaseCancelTransaction], tx)
    }
  }

  private def assertTxs(first: LeaseCancelTransaction, second: LeaseCancelTransaction): Unit = {
    first.leaseId shouldEqual second.leaseId
    first.fee shouldEqual second.fee
    first.signature shouldEqual second.signature
    first.bytes shouldEqual second.bytes
  }

}
