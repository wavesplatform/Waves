package com.wavesplatform.state.patch

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils._
import com.wavesplatform.state.patch.CancelAllLeases.CancelledLeases
import com.wavesplatform.state.{Blockchain, Diff, Portfolio}

case object CancelLeaseOverflow extends PatchAtHeight('W' -> 795000) {
  def apply(blockchain: Blockchain): Diff = {
    val patch = readPatchData[CancelledLeases]()
    val pfs = patch.balances.map {
      case (address, lb) =>
        Address.fromString(address).explicitGet() -> Portfolio(lease = lb)
    }
    Diff.empty.copy(portfolios = pfs, leaseState = patch.leaseStates)
  }
}
