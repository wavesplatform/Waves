package com.wavesplatform.state.patch

import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.state.patch.CancelAllLeases.CancelledLeases
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import play.api.libs.json.{Json, OFormat}

case class CancelAllLeases(blockchain: Blockchain) extends DiffPatchFactory {
  val height: Int = AddressScheme.current.chainId.toChar match {
    case 'W' => 462000
    case 'T' => 51500
    case _   => 0
  }

  def apply(): Diff = {
    val patch = PatchLoader.read[CancelledLeases](this)
    val pfs = patch.balances.map {
      case (address, lb) =>
        Address.fromString(address).explicitGet() -> Portfolio(lease = lb)
    }
    val cancelledLeases =
      for {
        id      <- patch.cancelledLeases.map(str => ByteStr.decodeBase58(str).get)
        details <- blockchain.leaseDetails(id)
      } yield (id, details.copy(isActive = false))

    Diff.empty.copy(portfolios = pfs, leaseState = cancelledLeases.toMap)
  }
}

object CancelAllLeases {
  private[patch] case class CancelledLeases(balances: Map[String, LeaseBalance], cancelledLeases: Set[String])
  private[patch] object CancelledLeases {
    implicit val jsonFormat: OFormat[CancelledLeases] = Json.format[CancelledLeases]
  }
}
