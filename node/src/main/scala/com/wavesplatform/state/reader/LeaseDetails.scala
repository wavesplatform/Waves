package com.wavesplatform.state.reader

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.TxPositiveAmount

object LeaseDetails {
  sealed trait Status
  object Status {
    case object Active                                             extends Status
    final case class Cancelled(height: Int, txId: Option[ByteStr]) extends Status
    final case class Expired(height: Int)                          extends Status

    implicit class StatusExt(val status: Status) extends AnyVal {
      def cancelHeight: Option[Int] = status match {
        case Status.Cancelled(height, _) => Some(height)
        case Status.Expired(height)      => Some(height)
        case Status.Active               => None
      }

      def cancelTransactionId: Option[ByteStr] = status match {
        case Status.Cancelled(_, txId) => txId
        case _                         => None
      }
    }

  }
}

case class LeaseDetails(sender: PublicKey, recipientAddress: Address, amount: TxPositiveAmount, status: LeaseDetails.Status, sourceId: ByteStr, height: Int) {
  def isActive: Boolean = status == LeaseDetails.Status.Active
}
