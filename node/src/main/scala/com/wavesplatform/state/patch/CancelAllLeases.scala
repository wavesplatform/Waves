package com.wavesplatform.state.patch

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import play.api.libs.json.{Json, OFormat}

case object CancelAllLeases extends PatchAtHeight('W' -> 462000, 'T' -> 51500) {
  private lazy val height: Int = patchHeight.getOrElse(0)

  private[patch] case class LeaseData(senderPublicKey: String, amount: Long, recipient: String, id: String)
  private[patch] case class CancelledLeases(balances: Map[String, LeaseBalance], cancelledLeases: Seq[LeaseData]) {
    val leaseStates: Map[ByteStr, LeaseDetails] = cancelledLeases.map { data =>
      val sender    = PublicKey(ByteStr.decodeBase58(data.senderPublicKey).get)
      val recipient = Address.fromString(data.recipient).explicitGet()
      val id        = ByteStr.decodeBase58(data.id).get
      (id, LeaseDetails(sender, recipient, data.amount, status = LeaseDetails.Status.Expired(height), id, height))
    }.toMap
  }
  private[patch] object CancelledLeases {
    implicit val dataFormat: OFormat[LeaseData]       = Json.format[LeaseData]
    implicit val jsonFormat: OFormat[CancelledLeases] = Json.format[CancelledLeases]
  }

  def apply(blockchain: Blockchain): Diff = {
    val patch = readPatchData[CancelledLeases]()
    Diff.empty.copy(portfolios = patch.balances.map {
      case (address, lb) =>
        Address.fromString(address).explicitGet() -> Portfolio(lease = lb)
    }, leaseState = patch.leaseStates)
  }
}
