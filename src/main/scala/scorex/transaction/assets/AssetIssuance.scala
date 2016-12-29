package scorex.transaction.assets

import scorex.transaction.SignedTransaction

/*
  Issue or Reissue of Asset
 */
trait AssetIssuance extends SignedTransaction {
  val assetId: Array[Byte]
  val reissuable: Boolean
  val quantity: Long
}
