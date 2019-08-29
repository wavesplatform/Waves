package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.{Account, Transaction, WithId, WithSignature}

case class UnknownTransaction(
  fee: Long,
  timestamp: Long,
  height: Int,
  `type`: Byte,
  version: Byte,
  signature: ByteString,
  id: ByteString,
  senderPublicKey: Account
) extends Transaction with WithSignature with WithId