package com.wavesplatform.state.patch

import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.state.{Diff, _}

trait CancelInvalidLeaseInLike {
  def apply(): Diff = {
    val pfs = PatchLoader.read[Map[String, LeaseBalance]](this).map {
      case (address, lb) =>
        Address.fromString(address).explicitGet() -> Portfolio(lease = lb)
    }
    Diff.empty.copy(portfolios = pfs)
  }
}

case object CancelInvalidLeaseIn extends DiffPatchFactory with CancelInvalidLeaseInLike {
  val height: Int = AddressScheme.current.chainId.toChar match {
    case 'W' => 1060000
    case _ => 0
  }
}
