package com.wavesplatform.state.reader

import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr

object LeaseDetails {
  sealed trait Status
  object Status {
    case object Active                                         extends Status
    final case class CancelledByTx(height: Int, txId: ByteStr) extends Status
    final case class CancelledAt(height: Int)                  extends Status

    def getCancelHeight(status: Status): Option[Int] = status match {
      case Status.CancelledByTx(height, _) => Some(height)
      case Status.CancelledAt(height)      => Some(height)
      case Status.Active                   => None
    }

    def getCancelTransactionId(status: Status): Option[ByteStr] = status match {
      case Status.CancelledByTx(_, txId) => Some(txId)
      case _ => None
    }
  }
}

case class LeaseDetails(sender: PublicKey, recipient: AddressOrAlias, sourceId: ByteStr, amount: Long, status: LeaseDetails.Status) {
  def isActive: Boolean = status == LeaseDetails.Status.Active
}
