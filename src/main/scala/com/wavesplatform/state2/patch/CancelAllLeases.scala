package com.wavesplatform.state2.patch

import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}

object CancelAllLeases {
  def apply(s: SnapshotStateReader): Diff = {

    def invertLeaseInfo(l: LeaseInfo): LeaseInfo = LeaseInfo(-l.leaseIn, -l.leaseOut)

    val portfolioUpd = s.accountPortfolios
      .collect { case (acc, pf) if pf.leaseInfo != LeaseInfo.empty =>
        acc -> Portfolio(0, invertLeaseInfo(pf.leaseInfo), Map.empty)
      }

    Diff.empty.copy(
      portfolios = portfolioUpd,
      leaseState = s.activeLeases().map(_ -> false).toMap)
  }

}
