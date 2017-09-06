package com.wavesplatform.state2.patch

import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2.{Diff, LeaseBalance, Portfolio}
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.lease.LeaseTransaction
import scorex.utils.ScorexLogging

object CancelLeaseOverflow extends ScorexLogging {
  def apply(s: SnapshotStateReader): Diff = {
    log.info("Cancelling all lease overflows for sender")

    val addressesWithLeaseOverflow = s.collectPortfolios { p => p.balance < p.lease.out }
    val leasesToCancel = addressesWithLeaseOverflow.keys.flatMap { a =>
      s.addressTransactions(a, Set(TransactionType.LeaseTransaction), 0, Int.MaxValue).collect {
        case (_, lt: LeaseTransaction) if lt.sender.toAddress == a => lt.id()
      }
    }

    log.info(s"Finished cancelling all lease overflows for sender. Transactions: ${leasesToCancel.mkString(", ")}\n${addressesWithLeaseOverflow.mkString("\n")}")

    Diff(transactions = Map.empty,
      portfolios = addressesWithLeaseOverflow.mapValues(p => Portfolio(0, LeaseBalance(0, -p.lease.out), Map.empty)),
      issuedAssets = Map.empty,
      aliases = Map.empty,
      orderFills = Map.empty,
      leaseState = leasesToCancel.map(_ -> false).toMap,
      scripts = Map.empty)
  }
}
