package com.wavesplatform.state.patch

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.state.*

case object CancelInvalidLeaseIn extends PatchAtHeight('W' -> 1060000) {
  def apply(blockchain: Blockchain): StateSnapshot =
    StateSnapshot.ofLeaseBalances(readPatchData[Map[Address, LeaseBalance]](), blockchain).explicitGet()
}
