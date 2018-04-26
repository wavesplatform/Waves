package scorex.transaction.base

import com.wavesplatform.state.ByteStr

trait ReissueTxBase extends TxBase {
  def assetId: ByteStr
  def quantity: Long
  def reissuable: Boolean
}
