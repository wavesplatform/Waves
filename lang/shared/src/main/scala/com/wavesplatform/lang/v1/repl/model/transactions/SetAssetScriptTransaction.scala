package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.{Account, Transaction, WithId, WithProofs}

object SetAssetScriptTransaction {
  val SET_ASSET_SCRIPT = 15
  val MAX_TX_SIZE      = 1024
}

case class SetAssetScriptTransaction(
  id: ByteString,
  fee: Long,
  timestamp: Long,
  height: Int,
  `type`: Byte,
  version: Byte,
  proofs: List[ByteString],
  senderPublicKey: Account,
  script: String,
  assetId: String
) extends Transaction with WithProofs with WithId
