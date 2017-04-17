package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.lease.LeaseTransaction

class LeaseTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Lease transaction serialization roundtrip") {
    forAll(leaseGenerator) { tx: LeaseTransaction =>
      val recovered = LeaseTransaction.parseBytes(tx.bytes).get

      assertTxs(recovered, tx)
    }
  }

  property("Lease transaction from TypedTransaction") {
    forAll(leaseGenerator) { tx: LeaseTransaction =>
      val recovered = TypedTransaction.parseBytes(tx.bytes).get

      assertTxs(recovered.asInstanceOf[LeaseTransaction], tx)
    }
  }

  private def assertTxs(first: LeaseTransaction, second: LeaseTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.recipient shouldEqual second.recipient
    first.amount shouldEqual second.amount
    first.fee shouldEqual second.fee
    first.signature shouldEqual second.signature
    first.bytes shouldEqual second.bytes
  }
}
