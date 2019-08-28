package com.wavesplatform.lang.v1.repl.model.transactions

import java.nio.ByteBuffer

import com.fasterxml.jackson.annotation.JsonProperty
import com.wavesplatform.lang.v1.repl.model.{Account, WithSignature}

object LeaseTransactionV1 {
  val LEASE = 8
}

case class LeaseTransactionV1(
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
