package com.wavesplatform.transaction
import com.wavesplatform.account.AddressOrAlias
import com.wavesplatform.account.protobuf.Recipient
import com.wavesplatform.common.state.ByteStr

//noinspection TypeAnnotation
package object protobuf {
  type PBTransaction = com.wavesplatform.transaction.protobuf.Transaction
  val PBTransaction = com.wavesplatform.transaction.protobuf.Transaction

  type PBSignedTransaction = com.wavesplatform.transaction.protobuf.SignedTransaction
  val PBSignedTransaction = com.wavesplatform.transaction.protobuf.SignedTransaction

  type VanillaTransaction = com.wavesplatform.transaction.Transaction
  val VanillaTransaction = com.wavesplatform.transaction.Transaction

  type VanillaSignedTransaction = com.wavesplatform.transaction.SignedTransaction

  type VanillaAssetId = com.wavesplatform.transaction.AssetId

  type PBAssetId = com.wavesplatform.transaction.protobuf.AssetId
  val PBAssetId = com.wavesplatform.transaction.protobuf.AssetId

  def toAmount(assetAmount: Amount): (Long, Option[ByteStr]) = ???
  def toAddressOrAlias(recipient: Recipient): AddressOrAlias = ???
}
