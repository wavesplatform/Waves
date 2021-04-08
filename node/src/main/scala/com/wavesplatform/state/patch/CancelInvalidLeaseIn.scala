package com.wavesplatform.state.patch

import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.state.{Diff, _}

case object CancelInvalidLeaseIn extends DiffPatchFactory {
  val height: Int = AddressScheme.current.chainId.toChar match {
    case 'W' => 1060000
    case _   => 0
  }

  def apply(): Diff = {
    import PatchLoader._
    Diff.empty.copy(portfolios = read[Map[String, LeaseBalance]](this).map {
      case (address, lb) =>
        Address.fromString(address).explicitGet() -> Portfolio(lease = lb)
    })
  }
}
