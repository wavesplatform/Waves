package com.wavesplatform.state.patch

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.state._

case object CancelInvalidLeaseIn extends PatchAtHeight('W' -> 1060000) {
  def apply(blockchain: Blockchain): Diff = {
    Diff.empty.copy(portfolios = readPatchData[Map[String, LeaseBalance]]().map {
      case (address, lb) =>
        Address.fromString(address).explicitGet() -> Portfolio(lease = lb)
    })
  }
}
