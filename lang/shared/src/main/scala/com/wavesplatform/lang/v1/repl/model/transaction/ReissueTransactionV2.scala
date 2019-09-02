package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.{Account, ByteString, WithProofs}

object ReissueTransactionV2 {
  val REISSUE             = 5
  private val MAX_TX_SIZE = 1024
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
