package com.wavesplatform.state.patch

import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.state.patch.CancelAllLeases.CancelledLeases
import com.wavesplatform.state.{Diff, Portfolio}

case object CancelLeaseOverflow extends DiffPatchFactory {
  val height: Int = AddressScheme.current.chainId.toChar match {
    case 'W' => 795000
    case _   => 0
  }

  def apply(): Diff = {
    val patch = PatchLoader.read[CancelledLeases](this)
    val pfs = patch.balances.map {
      case (address, lb) =>
        Address.fromString(address).explicitGet() -> Portfolio(lease = lb)
    }
    val leasesToCancel = patch.cancelledLeases.map(str => ByteStr.decodeBase58(str).get)
    Diff.empty // TODO
  }
}
