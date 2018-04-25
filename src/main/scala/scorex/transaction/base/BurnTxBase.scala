package scorex.transaction.base

import com.wavesplatform.state.ByteStr

trait BurnTxBase extends TxBase {
  def assetId: ByteStr
  def amount: Long
}
