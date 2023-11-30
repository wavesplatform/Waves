package com.wavesplatform.state.patch

import cats.implicits.catsSyntaxSemigroup
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.state.patch.CancelAllLeases.CancelledLeases
import com.wavesplatform.state.{Blockchain, StateSnapshot}

case object CancelLeaseOverflow extends PatchAtHeight('W' -> 795000) {
  def apply(blockchain: Blockchain): StateSnapshot = {
    val patch = readPatchData[CancelledLeases]()
    StateSnapshot.ofLeaseBalances(patch.balances, blockchain).explicitGet() |+| StateSnapshot(cancelledLeases = patch.leaseStates)
  }
}
