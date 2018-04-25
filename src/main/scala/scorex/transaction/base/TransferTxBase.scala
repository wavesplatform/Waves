package scorex.transaction.base

import scorex.account.AddressOrAlias
import scorex.transaction.{AssetId, Transaction}

trait TransferTxBase { _: Transaction =>
  def recipient: AddressOrAlias
  def assetId: Option[AssetId]
  def feeAssetId: Option[AssetId]
  def amount: Long
  def attachment: Array[Byte]
}
