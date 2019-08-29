package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.{Account, WithProofs, WithSignature}

object LeaseCancelTransactionV2 {
  val LEASE_CANCEL = 9
  private val MAX_TX_SIZE = 1024
}

case class LeaseCancelTransactionV2(
  leaseId: String,
  proofs: List[ByteString],
  senderPublicKey: Account,
  id: ByteString,
  fee: Long,
  timestamp: Long,
  height: Int,
  `type`: Byte,
  version: Byte
) extends LeaseCancelTransaction with WithProofs