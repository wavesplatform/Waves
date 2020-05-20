package com.wavesplatform.state.patch

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state._

case object SwapHijackedLeases extends DiffPatchFactory with CancelInvalidLeaseInLike {
  override def height: Int = 0
  override def isApplicable(b: Blockchain): Boolean =
    AddressScheme.current.chainId.toChar == 'W' && b.activatedFeatures.get(BlockchainFeatures.BlockV5.id).contains(b.height)
}
