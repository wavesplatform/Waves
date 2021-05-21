package com.wavesplatform.state.reader

import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr

object LeaseDetails {
  sealed trait Status
  object Status {
    case object Active                                     extends Status
    final case class Cancelled(height: Int, txId: ByteStr) extends Status
    final case class Expired(height: Int)                  extends Status

    def getCancelHeight(status: Status): Option[Int] = status match {
      case Status.Cancelled(height, _) => Some(height)
      case Status.Expired(height)      => Some(height)
      case Status.Active               => None
    }

    def getCancelTransactionId(status: Status): Option[ByteStr] = status match {
      case Status.Cancelled(_, txId) => Some(txId)
      case _                         => None
    }
  }
}

case class LeaseDetails(sender: PublicKey, recipient: AddressOrAlias, amount: Long, status: LeaseDetails.Status, sourceId: ByteStr, height: Int) {
  def isActive: Boolean = status == LeaseDetails.Status.Active
}
