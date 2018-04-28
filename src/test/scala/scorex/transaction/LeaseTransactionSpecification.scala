package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.lease.LeaseTransactionV1

class LeaseTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Lease transaction serialization roundtrip") {
    forAll(leaseGen) { tx: LeaseTransactionV1 =>
      val recovered = LeaseTransactionV1.parseBytes(tx.bytes()).get
      assertTxs(recovered, tx)
    }
  }

  property("Lease transaction from TransactionParser") {
    forAll(leaseGen) { tx: LeaseTransactionV1 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[LeaseTransactionV1], tx)
    }
  }

  private def assertTxs(first: LeaseTransactionV1, second: LeaseTransactionV1): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.recipient.stringRepr shouldEqual second.recipient.stringRepr
    first.amount shouldEqual second.amount
    first.fee shouldEqual second.fee
    first.signature shouldEqual second.signature
    first.bytes() shouldEqual second.bytes()
  }
}
