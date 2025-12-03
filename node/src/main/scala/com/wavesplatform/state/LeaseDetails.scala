package com.wavesplatform.state

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.state.LeaseDetails.Status as status
import com.wavesplatform.transaction.TxPositiveAmount
import play.api.libs.json.{Json, Writes}

object LeaseDetails {

  enum Status {
    case Active

    case Cancelled(height: Height, txId: Option[TransactionId]) extends Status, Status.Inactive
    case Expired(height: Height)                                extends Status, Status.Inactive

  }
  object Status {
    sealed trait Inactive

    extension (status: Status) {
      def cancelHeight: Option[Height] = status match {
        case Status.Cancelled(height, _) => Some(height)
        case Status.Expired(height)      => Some(height)
        case Status.Active               => None
      }

      def cancelTransactionId: Option[TransactionId] = status match {
        case Status.Cancelled(_, txId) => txId
        case _                         => None
      }
    }

    given Writes[Cancelled] = Json.writes

    given Writes[Expired] = Json.writes

    given Writes[Status.Inactive] = Json.writes
  }
}

case class LeaseDetails(
    static: LeaseStaticInfo,
    status: LeaseDetails.Status
) {
  def isActive: Boolean         = status == LeaseDetails.Status.Active
  def sender: PublicKey         = static.sender
  def sourceId: TransactionId   = static.sourceId
  def amount: TxPositiveAmount  = static.amount
  def height: Height            = static.height
  def recipientAddress: Address = static.recipientAddress
}
