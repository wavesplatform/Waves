package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.{Account, ByteString, WithSignature}

object LeaseTransactionV2 {
  val LEASE = 8
  val MAX_TX_SIZE = 1024
}

case class LeaseTransactionV2(
  recipient: String,
  amount: Long,
  fee: Long,
  timestamp: Long,
  signature: ByteString,
  senderPublicKey: Account,
  id: ByteString,
  height: Int,
  `type`: Byte,
  version: Byte
) extends LeaseTransaction with WithSignature
