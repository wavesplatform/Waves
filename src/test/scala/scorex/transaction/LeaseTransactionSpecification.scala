package scorex.transaction

import com.wavesplatform.OldTransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.lease.LeaseTransaction

class LeaseTransactionSpecification extends PropSpec with PropertyChecks with Matchers with OldTransactionGen {

  property("Lease transaction serialization roundtrip") {
    forAll(leaseGen) { tx: LeaseTransaction =>
      val recovered = LeaseTransaction.parseBytes(tx.bytes()).get
      assertTxs(recovered, tx)
    }
  }

  property("Lease transaction from TransactionParser") {
    forAll(leaseGen) { tx: LeaseTransaction =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[LeaseTransaction], tx)
    }
  }

  private def assertTxs(first: LeaseTransaction, second: LeaseTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.recipient.stringRepr shouldEqual second.recipient.stringRepr
    first.amount shouldEqual second.amount
    first.fee shouldEqual second.fee
    first.signature shouldEqual second.signature
    first.bytes() shouldEqual second.bytes()
  }
}
