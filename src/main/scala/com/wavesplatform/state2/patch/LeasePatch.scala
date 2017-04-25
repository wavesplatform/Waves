package com.wavesplatform.state2.patch

import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}

object LeasePatch {
  def apply(s: StateReader): Diff = {

    def invertLeaseInfo(l: LeaseInfo): LeaseInfo = LeaseInfo(-l.leaseIn, -l.leaseOut )

    val portfolioUpd = s.accountPortfolios
      .filter { case (_, pf) => pf.leaseInfo != LeaseInfo.empty }
      .map { case (acc, pf) => acc -> Portfolio(0, invertLeaseInfo(pf.leaseInfo), Map.empty) }

    Diff(transactions = Map.empty,
      portfolios = portfolioUpd,
      issuedAssets = Map.empty,
      aliases = Map.empty,
      paymentTransactionIdsByHashes = Map.empty,
      previousExchangeTxs = Map.empty,
      patchExtraLeaseIdsToCancel = s.activeLeases())
  }

}
