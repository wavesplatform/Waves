package com.wavesplatform.state.patch

import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.utils.ScorexLogging

object CancelAllLeases extends ScorexLogging {
  private def invertLeaseInfo(p: Portfolio) = Portfolio(0, LeaseBalance(-p.lease.in, -p.lease.out), Map.empty)

  def apply(blockchain: Blockchain): Diff = {
    log.info("Collecting all active leases")
    val leasesToCancel = blockchain.allActiveLeases.map(_.id() -> false).toMap
    leasesToCancel.foreach(id => log.info(s"Cancelling lease $id"))
    val portfolios = blockchain.collectLposPortfolios { case (_, p) if p.lease != LeaseBalance.empty => invertLeaseInfo(p) }
    portfolios.keys.foreach(addr => log.info(s"Resetting lease balance for $addr"))
    log.info("Finished collecting all active leases")

    Diff.empty.copy(portfolios = portfolios, leaseState = leasesToCancel)
  }
}
