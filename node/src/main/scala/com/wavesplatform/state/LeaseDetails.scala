package com.wavesplatform.state

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.TxPositiveAmount
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.protobuf as pb
import com.wavesplatform.database.protobuf.LeaseDetails.Status.*
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.protobuf.{ByteStrExt, ByteStringExt}
import com.wavesplatform.state.reader.LeaseDetails.Status

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
  }

  def fromProtobuf(d: pb.LeaseDetails): LeaseDetails =
    LeaseDetails(
      d.senderPublicKey.toPublicKey,
      PBRecipients.toAddressOrAlias(d.recipient.get, AddressScheme.current.chainId).explicitGet(),
      d.amount,
      d.status match {
        case Active(_)                                             => Status.Active
        case Expired(pb.LeaseDetails.Expired(height, _))           => Status.Expired(height)
        case Cancelled(pb.LeaseDetails.Cancelled(height, txId, _)) => Status.Cancelled(height, Some(txId.toByteStr).filter(!_.isEmpty))
        case Empty                                                 => ???
      },
      d.sourceId.toByteStr,
      d.height
    )
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

  def toProtobuf: pb.LeaseDetails =
    pb.LeaseDetails(
      ByteString.copyFrom(sender.arr),
      Some(PBRecipients.create(recipient)),
      amount,
      ByteString.copyFrom(sourceId.arr),
      height,
      status match {
        case Status.Active                        => Active(com.google.protobuf.empty.Empty())
        case Status.Cancelled(height, cancelTxId) => Cancelled(pb.LeaseDetails.Cancelled(height, cancelTxId.fold(ByteString.EMPTY)(_.toByteString)))
        case Status.Expired(height)               => Expired(pb.LeaseDetails.Expired(height))
      }
    )
}
