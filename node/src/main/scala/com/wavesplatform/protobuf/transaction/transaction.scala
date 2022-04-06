package com.wavesplatform.protobuf

//noinspection TypeAnnotation
package object transaction {
  type PBOrder = com.wavesplatform.protobuf.order.Order
  val PBOrder = com.wavesplatform.protobuf.order.Order

  type VanillaOrder = com.wavesplatform.transaction.assets.exchange.Order
  val VanillaOrder = com.wavesplatform.transaction.assets.exchange.Order

  type PBTransaction = com.wavesplatform.protobuf.transaction.Transaction
  val PBTransaction = com.wavesplatform.protobuf.transaction.Transaction

  type PBSignedTransaction = com.wavesplatform.protobuf.transaction.SignedTransaction
  val PBSignedTransaction = com.wavesplatform.protobuf.transaction.SignedTransaction

  type VanillaTransaction = com.wavesplatform.transaction.Transaction
  val VanillaTransaction = com.wavesplatform.transaction.Transaction

  type VanillaAssetId = com.wavesplatform.transaction.Asset
}
