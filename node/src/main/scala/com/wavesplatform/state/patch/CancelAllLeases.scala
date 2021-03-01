package com.wavesplatform.state.patch

import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{Diff, LeaseBalance, Portfolio}
import play.api.libs.json.Json

case object CancelAllLeases extends DiffPatchFactory {
  val height: Int = AddressScheme.current.chainId.toChar match {
    case 'W' => 462000
    case 'T' => 51500
    case _   => 0
  }

  private[patch] case class LeaseData(senderPublicKey: String, amount: Long, recipient: String, id: String)
  private[patch] case class CancelledLeases(balances: Map[String, LeaseBalance], cancelledLeases: Seq[LeaseData]) {
    val leaseStates: Map[ByteStr, LeaseDetails] = cancelledLeases.map { data =>
      val sender    = PublicKey(ByteStr.decodeBase58(data.senderPublicKey).get)
      val recipient = Address.fromString(data.recipient).explicitGet()
      val id        = ByteStr.decodeBase58(data.id).get
      (id, LeaseDetails(sender, recipient, id, data.amount, isActive = false))
    }.toMap
  }
  private[patch] object CancelledLeases {
    implicit val dataFormat = Json.format[LeaseData]
    implicit val jsonFormat = Json.format[CancelledLeases]
  }

  def apply(): Diff = {
    val patch = PatchLoader.read[CancelledLeases](this)
    val pfs = patch.balances.map {
      case (address, lb) =>
        Address.fromString(address).explicitGet() -> Portfolio(lease = lb)
    }
    Diff.empty.copy(portfolios = pfs, leaseState = patch.leaseStates)
  }
}
