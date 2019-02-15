package com.wavesplatform.transaction

//noinspection TypeAnnotation
package object protobuf {
  type PBTransaction = com.wavesplatform.transaction.protobuf.Transaction
  val PBTransaction = com.wavesplatform.transaction.protobuf.Transaction

  type VanillaTransaction = com.wavesplatform.transaction.Transaction
  val VanillaTransaction = com.wavesplatform.transaction.Transaction

  type VanillaAssetId = com.wavesplatform.transaction.AssetId

  type PBAssetId = com.wavesplatform.transaction.protobuf.AssetId
  val PBAssetId = com.wavesplatform.transaction.protobuf.AssetId
}
