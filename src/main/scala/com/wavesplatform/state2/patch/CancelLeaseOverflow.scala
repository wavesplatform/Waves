package com.wavesplatform.state2.patch

import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2.{ByteStr, Diff, LeaseBalance, Portfolio}
import scorex.account.Address
import cats.implicits._
import cats.kernel.Monoid
import scorex.utils.ScorexLogging

object CancelLeaseOverflow extends ScorexLogging {
  def apply(s: SnapshotStateReader): Diff = {
    log.debug("Cancelling all lease overflows for sender")

    val (status, portfolio) = s.activeLeases.foldLeft((Map.newBuilder[ByteStr, Boolean], Map.empty[Address, Portfolio])) {
      case ((st, p), lt) =>
        val senderPortfolio = s.portfolio(lt.sender.toAddress)
        if (senderPortfolio.lease.out > senderPortfolio.balance) {
          st += lt.id() -> false

          (st, Monoid.combine(p, Map(lt.sender.toAddress -> Portfolio(0, LeaseBalance(0, -lt.amount), Map.empty))))
        }
        (st, p)
    }

    log.debug("Finished cancelling all lease overflows for sender")

    Diff(transactions = Map.empty,
      portfolios = portfolio,
      issuedAssets = Map.empty,
      aliases = Map.empty,
      orderFills = Map.empty,
      leaseState = status.result(),
      scripts = Map.empty)
  }
}
