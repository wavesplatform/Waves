package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.tx.TransferTransaction
import com.wavesplatform.lang.v1.repl.model.{Account, ByteString, WithProofs}

object TransferTransactionV2 {
  private val MAX_TX_SIZE = 1024
}

case class TransferTransactionV2(
  id: ByteString,
  proofs: List[ByteString],
  recipient: String,
  amount: Long,
  assetId: Option[String],
  feeAssetId: Option[String],
  attachment: ByteString,
  fee: Long,
  timestamp: Long,
  height: Int,
  `type`: Byte,
  version: Byte,
  senderPublicKey: Account
) extends TransferTransaction with WithProofs