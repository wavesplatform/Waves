package com.wavesplatform.state.patch

import com.wavesplatform.state.{Diff, _}
import scorex.utils.ScorexLogging

object CancelInvalidLeaseIn extends ScorexLogging {
  def v1(blockchain: Blockchain): Diff = {
    log.info("Collecting lease in overflows")

    val nonZeroLeaseIns = blockchain.collectLposPortfolios {
      case (_, p) if p.lease.in > 0 => p.lease.in
    }

    log.info(s"Collected ${nonZeroLeaseIns.size} lease recipients")

    val diff = for {
      (address, storedLeaseIn) <- nonZeroLeaseIns
      actualLeaseIn = blockchain
        .activeLeases(address)
        .collect { case (_, lt) if blockchain.resolveAliasEi(lt.recipient).contains(address) => lt.amount }
        .sum
      if actualLeaseIn != storedLeaseIn
    } yield address -> Portfolio(0, LeaseBalance(actualLeaseIn - storedLeaseIn, 0), Map.empty)

    log.info(s"Done collecting lease in overflows:\n${diff.mkString("\n")}")

    Diff.empty.copy(portfolios = diff)
  }

  def v2(blockchain: Blockchain): Diff = {
    log.info("Collecting lease in overflows")

    val allActiveLeases = blockchain.allActiveLeases

    log.info(s"Collected ${allActiveLeases.size} active leases")

    val leaseInBalances = allActiveLeases.toSeq
      .map(lt => blockchain.resolveAliasEi(lt.recipient).explicitGet() -> lt.amount)
      .groupBy(_._1)
      .mapValues(_.map(_._2).sum)

    log.info("Calculated active lease in balances")

    val diff = blockchain.collectLposPortfolios {
      case (addr, p) if p.lease.in != leaseInBalances.getOrElse(addr, 0L) =>
        log.info(s"$addr: actual = ${leaseInBalances.getOrElse(addr, 0L)}, stored: ${p.lease.in}")
        Portfolio(0, LeaseBalance(leaseInBalances.getOrElse(addr, 0L) - p.lease.in, 0), Map.empty)
    }

    log.info("Finished collecting lease in overflows")

    Diff.empty.copy(portfolios = diff)
  }
}
