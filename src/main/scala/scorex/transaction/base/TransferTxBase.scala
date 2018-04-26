package scorex.transaction.base

import scorex.account.AddressOrAlias
import scorex.transaction.AssetId

trait TransferTxBase extends TxBase {
  def recipient: AddressOrAlias
  def assetId: Option[AssetId]
  def feeAssetId: Option[AssetId]
  def amount: Long
  def attachment: Array[Byte]
}
