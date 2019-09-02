package com.wavesplatform.lang.v1.repl.model.tx

import com.wavesplatform.lang.v1.repl.model.{Account, ByteString}

case class TransferTransactionV2(
  id: ByteString,
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
  senderPublicKey: Account,
  proofs: List[ByteString]
) extends TransferTransaction
