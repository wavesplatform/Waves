package com.wavesplatform.lang.v1.repl.model.transaction

import java.nio.ByteBuffer
import java.util
import java.util.Collections

import com.fasterxml.jackson.annotation.JsonProperty
import com.wavesplatform.lang.v1.repl.model.{Account, WithProofs}

object TransferTransactionV2 {
  private val MAX_TX_SIZE = 1024
}

case class TransferTransactionV2(
  id: ByteString,
  proofs: List[ByteString],
  recipient: String,
  amount: Long,
  assetId: String,
  feeAssetId: String,
  attachment: ByteString,
  fee: Long,
  timestamp: Long,
  height: Int,
  `type`: Byte,
  version: Byte,
  senderPublicKey: Account
) extends TransferTransaction with WithProofs