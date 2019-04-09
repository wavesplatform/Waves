package com.wavesplatform.state.patch

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.lease.{LeaseTransaction, LeaseTransactionV1}
import com.wavesplatform.utils.ScorexLogging

object CancelLeaseOverflow extends ScorexLogging {
  def apply(blockchain: Blockchain): Diff = {
    log.info("Cancelling all lease overflows for sender")

    val addressesWithLeaseOverflow = blockchain.collectLposPortfolios {
      case (_, p) if p.balance < p.lease.out => Portfolio(0, LeaseBalance(0, -p.lease.out), Map.empty)
    }
    addressesWithLeaseOverflow.keys.foreach(addr => log.info(s"Resetting lease overflow for $addr"))

    val leasesToCancel = addressesWithLeaseOverflow.keys.flatMap { a =>
      blockchain
        .addressTransactions(a, Set(LeaseTransactionV1.typeId), Int.MaxValue, None)
        .explicitGet()
        .collect { case (_, lt: LeaseTransaction) if lt.sender.toAddress == a => lt.id() }
    }
    leasesToCancel.foreach(id => log.info(s"Cancelling lease $id"))

    log.info("Finished cancelling all lease overflows for sender")

    Diff.empty.copy(portfolios = addressesWithLeaseOverflow, leaseState = leasesToCancel.map(_ -> false).toMap)
  }
}
