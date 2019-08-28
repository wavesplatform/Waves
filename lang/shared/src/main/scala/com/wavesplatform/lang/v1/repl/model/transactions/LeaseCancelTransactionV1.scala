package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.{Account, WithSignature}

object LeaseCancelTransactionV1 {
  val LEASE_CANCEL = 9
}

case class LeaseCancelTransactionV1(
  leaseId: String,
  signature: ByteString,
  senderPublicKey: Account,
  id: ByteString,
  fee: Long,
  timestamp: Long,
  height: Int,
  `type`: Byte,
  version: Byte
) extends LeaseCancelTransaction with WithSignature