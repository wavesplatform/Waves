package com.wavesplatform.state2.patch

import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2.{Diff, LeaseBalance, Portfolio}
import scorex.utils.ScorexLogging

object CancelAllLeases extends ScorexLogging {
  private def invertLeaseInfo(p: Portfolio) = Portfolio(0, LeaseBalance(-p.lease.in, -p.lease.out), Map.empty)

  def apply(s: SnapshotStateReader): Diff = {
    log.info("Collecting all active leases")
    val leasesToCancel = s.allActiveLeases.map(_.id() -> false).toMap
    val portfolios = s.collectPortfolios(_.lease != LeaseBalance.empty).mapValues(invertLeaseInfo)
    log.info(s"Done collecting all active leases;\n${portfolios.mkString("\n")}")

    Diff(transactions = Map.empty,
      portfolios = portfolios,
      issuedAssets = Map.empty,
      aliases = Map.empty,
      orderFills = Map.empty,
      leaseState = leasesToCancel,
      scripts = Map.empty)
  }
}
