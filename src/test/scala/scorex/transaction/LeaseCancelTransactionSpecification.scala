package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseCancelTransactionV2}

class LeaseCancelTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Lease cancel serialization roundtrip") {
    forAll(leaseCancelGen.retryUntil(_.isInstanceOf[LeaseCancelTransactionV2])) { tx: LeaseCancelTransaction =>
      val recovered = tx.builder.parseBytes(tx.bytes()).get.asInstanceOf[LeaseCancelTransaction]
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
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

}
