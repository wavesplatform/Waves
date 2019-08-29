package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.{Account, WithProofs}

object ReissueTransactionV2 {
  val REISSUE             = 5
  private val MAX_TX_SIZE = KBYTE
}

case class ReissueTransactionV2(
    id: ByteString,
    proofs: List[ByteString],
    assetId: String,
    quantity: Long,
    isReissuable: Boolean,
    fee: Long,
    timestamp: Long,
    height: Int,
    `type`: Byte,
    version: Byte,
    senderPublicKey: Account
) extends WithProofs with ReissueTransaction
