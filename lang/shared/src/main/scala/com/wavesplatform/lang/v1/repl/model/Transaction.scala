package com.wavesplatform.lang.v1.repl.model

import com.wavesplatform.lang.v1.repl.model.transaction.ByteString
import upickle.default._

sealed trait Transaction extends Signable {
  def fee: Long
  def timestamp: Long
  def height: Int
  def `type`: Byte
  def version: Byte
}

object Transaction {
  val V1 = 1
  val V2 = 2

  implicit val r: Reader[Transaction] = macroR
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

object SponsorTransaction {
  implicit val r: Reader[SponsorTransaction] = macroR
}