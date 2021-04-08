package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.api.common.LeaseInfo.Status
import com.wavesplatform.common.state.ByteStr

object LeaseInfo {
  type Status = Status.Value
  //noinspection TypeAnnotation
  object Status extends Enumeration {
    val active   = Value(1)
    val canceled = Value(0)
    val expired = Value(2)
  }
}

case class LeaseInfo(
    leaseId: ByteStr,
    originTransactionId: ByteStr,
    sender: Address,
    recipient: Address,
    amount: Long,
    height: Int,
    status: Status,
    leaseCancelHeight: Option[Int] = None,
    leaseCancelTransactionId: Option[ByteStr] = None
)
