package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.{Account, ByteString, Transaction, WithId, WithProofs}

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

object SponsorTransaction {
  val SPONSOR = 14
}

