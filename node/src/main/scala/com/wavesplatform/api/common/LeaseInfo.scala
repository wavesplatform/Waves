package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.api.common.LeaseInfo.Status
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.patch.CancelLeasesToDisabledAliases
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.TxValidationError.GenericError

object LeaseInfo {
  type Status = Status.Value
  // noinspection TypeAnnotation
  object Status extends Enumeration {
    val Active   = Value(1)
    val Canceled = Value(0)
    val Expired  = Value(2)
  }

  def fromLeaseDetails(id: ByteStr, details: LeaseDetails, blockchain: Blockchain): LeaseInfo =
    LeaseInfo(
      id,
      details.sourceId,
      details.sender.toAddress,
      blockchain.resolveAlias(details.recipient).orElse(resolveDisabledAlias(id)).explicitGet(),
      details.amount,
      details.height,
      details.status match {
        case LeaseDetails.Status.Active          => LeaseInfo.Status.Active
        case LeaseDetails.Status.Cancelled(_, _) => LeaseInfo.Status.Canceled
        case LeaseDetails.Status.Expired(_)      => LeaseInfo.Status.Expired
      },
      details.status.cancelHeight,
      details.status.cancelTransactionId
    )

  private def resolveDisabledAlias(leaseId: ByteStr): Either[ValidationError, Address] =
    CancelLeasesToDisabledAliases.patchData
      .get(leaseId)
      .fold[Either[ValidationError, Address]](Left(GenericError("Unknown lease ID"))) { case (_, recipientAddress) =>
        Right(recipientAddress)
      }
}

case class LeaseInfo(
    id: ByteStr,
    originTransactionId: ByteStr,
    sender: Address,
    recipient: Address,
    amount: Long,
    height: Int,
    status: Status,
    cancelHeight: Option[Int] = None,
    cancelTransactionId: Option[ByteStr] = None
)
