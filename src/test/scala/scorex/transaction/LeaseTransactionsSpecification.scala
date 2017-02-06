package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

class LeaseTransactionsSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Lease serialization roundtrip") {
    forAll(leaseGenerator) { case (lease: LeaseTransaction, unlease: LeaseCancelTransaction) =>
      val recoveredLease = LeaseTransaction.parseBytes(lease.bytes).get
      recoveredLease.bytes shouldEqual lease.bytes

      val recoveredLeaseCancel = LeaseCancelTransaction.parseBytes(unlease.bytes).get
      recoveredLease.bytes shouldEqual lease.bytes
    }
  }

  property("Issue serialization from TypedTransaction") {
    forAll(leaseGenerator) { case (lease: LeaseTransaction, unlease: LeaseCancelTransaction) =>
      val recoveredLease = TypedTransaction.parseBytes(lease.bytes).get
      recoveredLease.bytes shouldEqual lease.bytes

      val recoveredLeaseCancel = TypedTransaction.parseBytes(unlease.bytes).get
      recoveredLease.bytes shouldEqual lease.bytes
    }
  }

}
