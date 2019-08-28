package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.{Account, WithId, WithProofs}

object DataTransaction {
  val DATA        = 12
  val MAX_TX_SIZE = 150 * 1024
}

case class DataTransaction(
    id: ByteString,
    data: List[DataEntry[_]],
    senderPublicKey: Account,
    proofs: List[ByteString],
    timestamp: Long,
    fee: Long,
    height: Int,
    `type`: Byte,
    version: Byte
) extends Transaction with WithProofs with WithId
