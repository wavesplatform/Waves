package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.{Account, Transaction, WithId, WithProofs}

object SponsorTransaction {
  val SPONSOR = 14
}

case class SponsorTransaction(
  fee: Long,
  timestamp: Long,
  height: Int,
  `type`: Byte,
  version: Byte,
  proofs: List[ByteString],
  id: ByteString,
  senderPublicKey: Account,
  assetId: String,
  minSponsoredAssetFee: Long
) extends Transaction with WithProofs with WithId