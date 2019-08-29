package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.{Account, Transaction, WithId, WithProofs}

object SetScriptTransaction {
  val SET_SCRIPT = 13
  val MAX_TX_SIZE = 10 * 1024
}

case class SetScriptTransaction(
  fee: Long,
  timestamp: Long,
  height: Int,
  `type`: Byte,
  version: Byte,
  proofs: List[ByteString],
  id: ByteString,
  senderPublicKey: Account,
  string: String
) extends Transaction with WithProofs with WithId