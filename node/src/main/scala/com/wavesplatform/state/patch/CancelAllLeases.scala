package com.wavesplatform.state.patch

import cats.implicits.catsSyntaxSemigroup
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.state.{Blockchain, LeaseBalance, LeaseDetails, StateSnapshot}
import play.api.libs.json.{Json, OFormat}

case object CancelAllLeases extends PatchAtHeight('W' -> 462000, 'T' -> 51500) {
  private[patch] case class LeaseData(senderPublicKey: String, amount: Long, recipient: String, id: String)

  private[patch] case class CancelledLeases(balances: Map[Address, LeaseBalance], cancelledLeases: Seq[LeaseData]) {
    private[this] val height: Int = patchHeight.getOrElse(0)
    val leaseStates: Map[ByteStr, LeaseDetails.Status.Inactive] = cancelledLeases.map { data =>
      (ByteStr.decodeBase58(data.id).get, LeaseDetails.Status.Expired(height))
    }.toMap
  }

  private[patch] object CancelledLeases {
    implicit val dataFormat: OFormat[LeaseData]       = Json.format[LeaseData]
    implicit val jsonFormat: OFormat[CancelledLeases] = Json.format[CancelledLeases]
  }

  def apply(blockchain: Blockchain): StateSnapshot = {
    val patch = readPatchData[CancelledLeases]()
    StateSnapshot.ofLeaseBalances(patch.balances, blockchain).explicitGet() |+| StateSnapshot(cancelledLeases = patch.leaseStates)
  }
}
