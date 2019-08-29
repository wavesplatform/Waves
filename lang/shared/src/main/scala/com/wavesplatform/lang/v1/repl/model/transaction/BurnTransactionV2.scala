package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.{Account, WithProofs}

object BurnTransactionV2 {
  val MAX_TX_SIZE = 1024
}

case class BurnTransactionV2(
  id: ByteString,
  proofs: List[ByteString],
  assetId: String,
  amount: Long,
  senderPublicKey: Account,
  fee: Long,
  timestamp: Long,
  height: Int,
  `type`: Byte,
  version: Byte
) extends BurnTransaction with WithProofs
