package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.{Account, Alias, ByteString, WithProofs}

object AliasTransactionV2 {
  val MAX_TX_SIZE = 1024
}

case class AliasTransactionV2(
    id: ByteString,
    senderPublicKey: Account,
    alias: Alias,
    fee: Long,
    timestamp: Long,
    height: Int,
    `type`: Byte,
    version: Byte,
    proofs: List[ByteString]
) extends AliasTransaction with WithProofs
