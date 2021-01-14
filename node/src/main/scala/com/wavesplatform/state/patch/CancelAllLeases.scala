package com.wavesplatform.state.patch

import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.state.{Diff, LeaseBalance, Portfolio}
import play.api.libs.json.Json

case object CancelAllLeases extends DiffPatchFactory {
  val height: Int = AddressScheme.current.chainId.toChar match {
    case 'W' => 462000
    case 'T' => 51500
    case _   => 0
  }

  private[patch] case class CancelledLeases(balances: Map[String, LeaseBalance], cancelledLeases: Set[String])
  private[patch] object CancelledLeases {
    implicit val jsonFormat = Json.format[CancelledLeases]
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
