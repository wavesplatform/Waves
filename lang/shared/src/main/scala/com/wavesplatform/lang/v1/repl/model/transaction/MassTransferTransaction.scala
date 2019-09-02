package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.{Account, ByteString, Transaction, Transfer, WithId, WithProofs}

object MassTransferTransaction {
  val MASS_TRANSFER = 11
  val MAX_TX_SIZE = 5 * 1024
}

case class MassTransferTransaction(
  id: ByteString,
  fee: Long,
  timestamp: Long,
  height: Int,
  `type`: Byte,
  version: Byte,
  proofs: List[ByteString],
  senderPublicKey: Account,
  transfers: List[Transfer],
  attachment: ByteString,
  assetId: String
) extends Transaction with WithProofs with WithId