package scorex.transaction.base

import monix.eval.Coeval
import scorex.transaction.AssetId
import scorex.transaction.smart.script.Script

trait IssueTxBase extends TxBase {
  def name: Array[Byte]
  def assetId: Coeval[AssetId]
  def description: Array[Byte]
  def quantity: Long
  def decimals: Byte
  def reissuable: Boolean
  def script: Option[Script]
}
