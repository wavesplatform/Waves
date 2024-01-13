package com.wavesplatform.state

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.TxPositiveAmount
import play.api.libs.json.{Format, Json, Writes}

object LeaseDetails {
  sealed trait Status
  object Status {
    case object Active                                             extends Status
    sealed trait Inactive                                          extends Status
    final case class Cancelled(height: Int, txId: Option[ByteStr]) extends Inactive
    final case class Expired(height: Int)                          extends Inactive

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

    implicit val byteStrFormat: Format[ByteStr]     = com.wavesplatform.utils.byteStrFormat
    implicit val writesCancelled: Writes[Cancelled] = Json.writes
    implicit val writesExpired: Writes[Expired]     = Json.writes
    implicit val writesInactive: Writes[Inactive]   = Json.writes
  }
}

case class LeaseDetails(
    static: LeaseStaticInfo,
    status: LeaseDetails.Status
) {
  def isActive: Boolean         = status == LeaseDetails.Status.Active
  def sender: PublicKey         = static.sender
  def sourceId: ByteStr         = static.sourceId
  def amount: TxPositiveAmount  = static.amount
  def height: Int               = static.height
  def recipientAddress: Address = static.recipientAddress
}
