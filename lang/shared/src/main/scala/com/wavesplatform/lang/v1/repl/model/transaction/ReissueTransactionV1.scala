package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.{Account, WithSignature}

case class ReissueTransactionV1(
  signature: ByteString,
  assetId: String,
  quantity: Long,
  isReissuable: Boolean,
  id: ByteString,
  fee: Long,
  timestamp: Long,
  height: Int,
  `type`: Byte,
  version: Byte,
  senderPublicKey: Account
) extends ReissueTransaction with WithSignature