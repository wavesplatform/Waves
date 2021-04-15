package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.api.common.LeaseInfo.Status
import com.wavesplatform.common.state.ByteStr

object LeaseInfo {
  type Status = Status.Value
  //noinspection TypeAnnotation
  object Status extends Enumeration {
    val Active   = Value(1)
    val Canceled = Value(0)
    val Expired  = Value(2)
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
