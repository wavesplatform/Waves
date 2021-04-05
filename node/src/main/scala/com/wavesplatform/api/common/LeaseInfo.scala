package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr

object LeaseInfo {
    type Status = Status.Value
    object Status extends Enumeration {
      val Active = Value(1)
      val Cancelled = Value(0)
    }

    case class TransactionRef(originTransactionId: ByteStr, height: Int)
  }

  case class LeaseInfo(
      leaseId: ByteStr,
      originTransactionId: ByteStr,
      sender: Address,
      recipient: Address,
      amount: Long,
      height: Int,
      status: LeaseInfo.Status,
      leaseTransactionRef: Option[LeaseInfo.TransactionRef],
      leaseCancelTransactionRef: Option[LeaseInfo.TransactionRef]
  )